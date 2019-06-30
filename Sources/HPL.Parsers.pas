unit HPL.Parsers;
(*
 * Copyright (c) 2000 - 2019 Yuriy Kotsarenko. All rights reserved.
 *
 * This program is free software; you can redistribute it and/or modify it under the terms of
 * the GNU General Public License version 2 as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *)
interface

{$INCLUDE HPL.Config.inc}

uses
  Classes, SysUtils, HPL.Types, HPL.DefineSymbols, HPL.Parameters, HPL.MethodParameters, HPL.DataTypes,
  HPL.Annotations, HPL.BasicTypes;

type
  TParserOption = (LeaveComments, LeaveUnsupportedSymbols, SeparateByType, DefineCommonSymbols, ShortenParameterNames,
    GuessInterfaces, SkipOutput);
  TParserOptions = set of TParserOption;

  TParserDebugOption = (StructParamSizes);
  TParserDebugOptions = set of TParserDebugOption;

const
  DefaultParserOptions = [TParserOption.LeaveComments, TParserOption.LeaveUnsupportedSymbols,
    TParserOption.DefineCommonSymbols, TParserOption.GuessInterfaces, TParserOption.ShortenParameterNames];

type
  THeaderParser = class(TCustomTextParser)
  private type
    TSection = (None, UnsupportedSymbols, Types, Constants, Variables, Functions);
    TSubSection = (None, Forwards, Enums, Structs, Redirects, Interfaces, Comments, GUIDs, Callbacks, SIDs, IIDs);

    TCodeSection = (None, Constants_Defines, Constants_Enums, Constants_GUID, Constants_SID, Constants_IID,
      Types_Forwards, Types_Redirects, Type_Enums, Types_Records, Types_Callbacks, Types_Interfaces, Variables,
      Functions);

    TLogicalOperator = (None, C_AND, C_OR);
    TComparisonOperator = (None, Equal, NotEqual, Bigger, Smaller, BiggerOrEqual, SmallerOrEqual);
    TCallingType = (Normal, WinStd);

    TExceptionClass = class of Exception;
  private
    FDefineSymbols: TDefineSymbols;
    FIgnoreSymbols: TStringList;

    FAnnotations: TAnnotations;
    FDataTypes: TDataTypes;
    FParameters: TParameters;
    FParameterNames: TStringList;
    FMethodParameters: TMethodParameters;
    FDataTypeAliases: TDataTypeAliases;

    FParameterHolderList: TParameterHolder;
    FParameterHolderListRoot: TParameterHolder;
    FObjectHandles: TStringList;

    FCode: array[TCodeSection] of StdString;
    FCodeSymbol: array[TCodeSection] of StdString;

    FFinalCode: StdString;

    FDebugText: StdString;

    FDefineLevel: Integer;
    FDefineLevelSkip: Integer;
    FSideSpace: Integer;

    FOptions: TParserOptions;
    FDebugOptions: TParserDebugOptions;

    FSection: TSection;
    FSubSection: TSubSection;
    FCodeSection: TCodeSection;
    FLineWidth: Integer;
    FLineLimit: Integer;

    procedure StartCodeSection(const ACodeSection: TCodeSection; const ACodeSymbol: StdString = '');
    procedure StartSection(const ASection: TSection; const ASubSection: TSubSection = TSubSection.None);

    function SideSpaceText(const SideSpaceOverride: Integer = -1): StdString;

    procedure RaiseException(const ExceptClass: TExceptionClass; const ExceptText: StdString;
      const ATextPos: Integer = -1);

    procedure ExpectedException(const ExpectedText: StdString; const ATextPos: Integer = -1);

    function RetrieveIntegerValue: StdString;
    function ProcessIdentifier: StdString;
    function ProcessNumericalValue: StdString;
    function ProcessIntegerValue: StdString;
    function ProcessGuidIntegerValue(const MinLetterCount: Integer): StdString;
    function ProcessNumericalValueOrIdentifier: StdString;
    procedure ProcessExpectedChar(const Ch: StdChar);
    function ProcessGUIDValue: StdString;

    function RetrieveLogicalOperator: TLogicalOperator;
    function RetrieveComparisonOperator: TComparisonOperator;
    function RetrieveExpressionOperator: StdString;
    function ProcessExpressionOperator: StdString;

    function AddPointerToDataType(const Text: StdString): StdString;
    function IsDataTypeComplex(const DataTypeName: StdString): Boolean;
    function IsDataTypeInterfaceGuess(const DataTypeName: StdString): Boolean;

    function SkipComments(const ALeaveComments: Boolean): Boolean;
    function SkipUnsupportedDirective(const Directive: StdString): Boolean;
    function SkipUnsupportedDirectives: Boolean;

    procedure ProcessConditionalIFDEF;
    procedure ProcessConditionalIFNDEF;
    procedure ProcessConditionalENDIF;
    procedure ProcessConditionalELSE;

    procedure ProcessExpressionIFOperator(var FinalResult, NegateNextValue, ResultDefined: Boolean;
      CurrentValue: Boolean; var LastOperator: TLogicalOperator);

    function ProcessExpressionIF(const ParenthesisLevel: Integer = 0): Boolean;
    procedure ProcessConditionalIF;

    procedure ProcessConditionalUNDEF;

    function ParseExpression(const DefineExpression: Boolean = True;
      const ParenthesisLevel: Integer = 0): StdString;

    procedure ProcessConditionalDEFINE;

    function ProcessConditionals: Boolean;

    procedure ProcessStatementTypedefINTERFACE(const IgnoreSecondPart: Boolean = False);

    procedure ProcessStatementTypedefENUM;

    function ProcessMultiWordDataType(const FirstWord: StdString): StdString;
    function TranslateDataType(const DataType, Translated: StdString;
      const PointerIndex, DereferenceCount: Integer): StdString;

    function GetAnnotationInfo(const Text: StdString; out Info: TAnnotationInfo): Boolean;
    function IsAnnotation(const Text: StdString): Boolean;

    procedure DereferencePointer(var PointerIndex, DereferenceCount: Integer);
    function ProcessPointerReferences: Integer;
    procedure UpdateParameterName(var ParameterName: StdString; const DataTypeName, TranslatedDataType: StdString;
      const PointerIndex, DereferenceCount: Integer; const ExistingParameters: TStrings);

    procedure ProcessStructUnionVariableName;

    function ProcessStructParameters(const BaseUnion: TUnionParameter = nil;
      const BaseStruct: TStructParameter = nil): Integer;
    procedure ProcessDeclarationsIntoRecordEntries(const BaseUnion: TUnionParameter = nil;
      const BaseStruct: TStructParameter = nil);

    function GetRecordEntriesText(const BaseNode: TParameterHolder): StdString;

    procedure SkipRemainingStatementSTRUCT;
    procedure ProcessRemainingAliases(const DataTypeName: StdString);

    procedure ProcessStatementTypedefSTRUCT(const IsUnionFromStart: Boolean = False);
    procedure ProcessStatementTypedefCALLBACK;
    procedure ProcessStatementTYPEDEF;

    procedure ProcessSkipOpenCloseBrackets(const StartsWithOpen: Boolean = True; const OpenBracket: StdChar = '(';
      const CloseBracket: StdChar = ')');

    function MethodDeclarationEndsWidthVoid: Boolean;
    function ProcessMethodParameterDeclaration(GenericParameterNameIndex: PInteger;
      out Declaration: TParameterDeclaration): Boolean;
    procedure ConvertParameterDeclarationIntoMethodParameter(const Declaration: TParameterDeclaration;
      out Parameter: TMethodParameter);
    procedure ConvertParameterDeclarationIntoStructParameter(const Declaration: TParameterDeclaration;
      const DataTypeOffset: Integer; out Parameter: TNormalParameter);

    procedure ProcessDeclarationMethodsParametersToList;
    function ProcessDeclarationMethodsParameters(out ANewLineWidth: Integer;
      const ALineWidth: Integer): string;
    procedure ProcessStatementMIDLInterfaceMethods;
    procedure ProcessStatementMIDLInterface;

    procedure ProcessStatementSTDInterfaceMethods;
    procedure ProcessStatementSTDInterface;
    procedure ProcessStatementSTDLegacyInterface;

    procedure ProcessStatementDefineGUID;

    procedure ProcessGlobalFunction;
  protected
    procedure AddText(const Text: string);
    procedure AddLine(const Text: string);
    procedure AddLineBreak;
    procedure AddDebugText(const Text: StdString);
  public
    constructor Create(const AOptions: TParserOptions = DefaultParserOptions);
    destructor Destroy; override;

    procedure DeclareObjectHandles;

    procedure Process;
    procedure CombineCode;
    procedure DumpDataTypeAliases;

    property DefineSymbols: TDefineSymbols read FDefineSymbols;
    property IgnoreSymbols: TStringList read FIgnoreSymbols;

    property Options: TParserOptions read FOptions write FOptions;
    property DebugOptions: TParserDebugOptions read FDebugOptions write FDebugOptions;

    property FinalCode: StdString read FFinalCode;
    property DebugText: StdString read FDebugText;
  end;

  EParserException = class(Exception);

  ECannotInterpret = class(EParserException);
  EIdentifierExpected = class(EParserException);
  ENumericalValueExpected = class(EParserException);
  EIntegerValueExpected = class(EParserException);
  ENumericalValueOrIdentifierExpected = class(EParserException);
  EGUIDValueExpected = class(EParserException);
  EExpressionOperatorExpected = class(EParserException);
  EMisplacedDirective = class(EParserException);
  EExpressionExpected = class(EParserException);
  EEnumerationExpected = class(EParserException);
  EDeclarationExpected = class(EParserException);
  EUnexpectedCode = class(EParserException);

  EOperatorWithoutPredicate = class(EExpressionExpected);

const
  ExceptionBase = '[%d, %d]: ';

resourcestring
  SCannotInterpret = ExceptionBase + 'Cannot interpret this code: %s';
  SIdentifierExpected = ExceptionBase + 'Identifier expected, but found: %s';
  SNumericalValueExpected = ExceptionBase + 'Numerical value expected, but found: %s';
  SIntegerValueExpected = ExceptionBase + 'Integer value expected, but found: %s';
  SNumericalValueOrIdentifierExpected = ExceptionBase + 'Numerical value or identifier expected, but found: %s';
  SGUIDValueExpected = ExceptionBase + 'GUID value expected, but found: %s';
  SExpressionOperatorExpected = ExceptionBase + 'Exception operator expected, but found: %s';
  SMisplacedDirective = ExceptionBase + 'Misplaced directive: %s';
  SExpressionExpected = ExceptionBase + 'Expression expected, but found: %s';
  SEnumerationExpected = ExceptionBase + 'Enumeration expected, but found: %s';
  SDeclarationExpected = ExceptionBase + 'Declaration expected, but found: %s';
  SOperatorWithoutPredicate = ExceptionBase + 'Need predicate, but got operator and then: %s';
  SUnexpectedCode = ExceptionBase + '%s expected, but found: %s';

implementation

uses
  HPL.ReservedWords;

const
  DefaultTabWidth = 2;
  CommonDataTypeVOID = 'void';
  CommonDataTypeKeyword = 'FAR';
  UnknownDataTypeSize = 1024;

constructor THeaderParser.Create(const AOptions: TParserOptions);
begin
  inherited Create;

  FObjectHandles := TStringList.Create;
  FObjectHandles.Sorted := True;

  FDefineSymbols := TDefineSymbols.Create;

  FIgnoreSymbols := TStringList.Create;
  FIgnoreSymbols.Sorted := True;
  FIgnoreSymbols.CaseSensitive := True;

  FAnnotations := TAnnotations.Create;
  FDataTypes := TDataTypes.Create;
  FParameters := TParameters.Create;

  FParameterNames := TStringList.Create;
  FParameterNames.Sorted := True;
  FParameterNames.CaseSensitive := False;

  FParameterHolderList := TParameterHolder.Create;
  FParameterHolderListRoot := TParameterHolder.Create(nil, False);

  FMethodParameters := TMethodParameters.Create;
  FDataTypeAliases := TDataTypeAliases.Create;

  FDefineLevelSkip := -1;
  FLineLimit := 110;

  FOptions := AOptions;
  FDebugOptions := [];

  if TParserOption.DefineCommonSymbols in FOptions then
  begin
    FDataTypes.Add('REFGUID', '', 'TGuid', 16, 1, TDataTypeGroup.Structure, TDataTypeRelation.&External);
    FDataTypes.Add('REFIID', '', 'TGuid', 16, 1, TDataTypeGroup.Structure, TDataTypeRelation.&External);
    FDataTypes.Add('GUID', '', 'TGuid', 16, 0, TDataTypeGroup.Structure, TDataTypeRelation.&External);
    FDataTypes.Add('CLSID', '', 'TGuid', 16, 0, TDataTypeGroup.Structure, TDataTypeRelation.&External);
    FDataTypes.Add('REFCLSID', '', 'TGuid', 16, 1, TDataTypeGroup.Structure, TDataTypeRelation.&External);
    FDataTypes.Add('LUID', '', 'TLuid', 8, 0, TDataTypeGroup.Structure, TDataTypeRelation.&External);

    FDataTypes.Add('SIZE', '', 'TSize', 8, 0, TDataTypeGroup.Structure, TDataTypeRelation.&External);
    FDataTypes.Add('IUnknown', '', 'IUnknown', DefaultPointerDataTypeSize, 0, TDataTypeGroup.&Interface,
      TDataTypeRelation.System);

    FDataTypes.Add('Point', '', 'Point2i', 8, 0, TDataTypeGroup.Structure, TDataTypeRelation.&External);
    FDataTypes.Add('PointF', '', 'Point2f', 8, 0, TDataTypeGroup.Structure, TDataTypeRelation.&External);
    FDataTypes.Add('Vector', '', 'Vector3f', 12, 0, TDataTypeGroup.Structure, TDataTypeRelation.&External);
    FDataTypes.Add('Vector4', '', 'Vector4f', 16, 0, TDataTypeGroup.Structure, TDataTypeRelation.&External);

    FDataTypes.Add('Rect', '', 'IntRect', 16, 0, TDataTypeGroup.Structure, TDataTypeRelation.&External);
    FDataTypes.Add('RectF', '', 'FloatRect', 16, 0, TDataTypeGroup.Structure, TDataTypeRelation.&External);
    FDataTypes.Add('Matrix3', '', 'Matrix3f', 36, 0, TDataTypeGroup.Structure, TDataTypeRelation.&External);
    FDataTypes.Add('Matrix', '', 'Matrix4f', 64, 0, TDataTypeGroup.Structure, TDataTypeRelation.&External);
    FDataTypes.Add('ColorRect', '', 'ColorRect', 4, 0, TDataTypeGroup.Structure, TDataTypeRelation.&External);

    FDataTypes.Add('Color', '', 'IntColor', 4, 0, TDataTypeGroup.Structure, TDataTypeRelation.&External);

    // Windows
    FDefineSymbols.Add('_WIN32');
    FDefineSymbols.Add('WIN32');

    // DirectX 7 headers
    FIgnoreSymbols.Add('INTERFACE');
    FIgnoreSymbols.Add('D3DAPI');

    // DirectX 10 headers
    FDefineSymbols.Add('__cplusplus');

    FIgnoreSymbols.Add('_FACDXGI');
    FIgnoreSymbols.Add('_FACD3D10');
    FIgnoreSymbols.Add('_FACD3D10DEBUG');

    FIgnoreSymbols.Add('__REQUIRED_RPCNDR_H_VERSION__');
    FIgnoreSymbols.Add('__REQUIRED_RPCSAL_H_VERSION__');

    FDataTypes.Add('TextureAttributes', '', 'TTextureAttributes', 4, 0, TDataTypeGroup.Enum, TDataTypeRelation.&External);
    FDataTypes.Add('TessellationWinding', '', 'TTessellationWinding', 4, 0, TDataTypeGroup.Enum, TDataTypeRelation.&External);
    FDataTypes.Add('TextAlignment', '', 'TTextAlignment', 4, 0, TDataTypeGroup.Enum, TDataTypeRelation.&External);
    FDataTypes.Add('TimerFunc', '', 'TTimerFunc', 4, 0, TDataTypeGroup.Callback, TDataTypeRelation.&External);
    FDataTypes.Add('ImageRegion', '', 'ImageRegion', 32, 0, TDataTypeGroup.Structure, TDataTypeRelation.&External);
  end;
end;

destructor THeaderParser.Destroy;
begin
  FDataTypeAliases.Free;
  FMethodParameters.Free;
  FParameterHolderListRoot.Free;
  FParameterHolderList.Free;
  FParameterNames.Free;
  FParameters.Free;
  FDataTypes.Free;
  FAnnotations.Free;
  FIgnoreSymbols.Free;
  FDefineSymbols.Free;
  FObjectHandles.Free;

  inherited;
end;

procedure THeaderParser.AddText(const Text: string);
begin
  if not (TParserOption.SkipOutput in FOptions) then
  begin
    if TParserOption.SeparateByType in FOptions then
      FCode[FCodeSection] := FCode[FCodeSection] + Text
    else
      FFinalCode := FFinalCode + Text;

    Inc(FLineWidth, Length(Text));
  end;
end;

procedure THeaderParser.AddLine(const Text: string);
begin
  if Length(Text) > 0 then
    AddText(Text + SLineBreak);
end;

procedure THeaderParser.AddLineBreak;
begin
  AddText(SLineBreak);
  FLineWidth := 0;
end;

procedure THeaderParser.AddDebugText(const Text: StdString);
var
  LinePos, HorizPos: Integer;
begin
  EstimateLineAndPosition(FTextPos, LinePos, HorizPos);
  FDebugText := FDebugText + '[' + IntToStr(LinePos) + ', ' + IntToStr(HorizPos) + '] ' + Text + SLineBreak;
end;

procedure THeaderParser.StartCodeSection(const ACodeSection: TCodeSection; const ACodeSymbol: StdString);
begin
  FCodeSection := ACodeSection;

  if not SameText(FCodeSymbol[FCodeSection], ACodeSymbol) then
  begin
    if Length(FCode[FCodeSection]) > 0 then
      FCode[FCodeSection] := FCode[FCodeSection] + SLineBreak;

    FCodeSymbol[FCodeSection] := ACodeSymbol;
  end;
end;

procedure THeaderParser.StartSection(const ASection: TSection; const ASubSection: TSubSection);
begin
  if not (TParserOption.SeparateByType in FOptions) then
  begin
    if ((FSection <> ASection) or (FSubSection <> ASubSection)) and (FSection <> TSection.None) then
      AddLineBreak
    else if (ASection = TSection.Types) and (ASubSection = TSubSection.Structs) then
      AddLineBreak;

    if FSection <> ASection then
    begin
      if not (TParserOption.SeparateByType in FOptions) then
        case ASection of
          TSection.Types:
            AddLine('type');

          TSection.Constants:
            AddLine('const');

          TSection.Variables:
            AddLine('var');
        end;
    end;
  end;

  FSection := ASection;
  FSubSection := ASubSection;

  if ASection <> TSection.Functions then
    FSideSpace := DefaultTabWidth
  else
    FSideSpace := 0;
end;

function THeaderParser.SideSpaceText(const SideSpaceOverride: Integer): StdString;
var
  I, LSideSpace: Integer;
begin
  if SideSpaceOverride <> -1 then
    LSideSpace := SideSpaceOverride
  else
    LSideSpace := FSideSpace;

  Result := '';

  for I := 0 to LSideSpace - 1 do
    Result := Result + #32;
end;

procedure THeaderParser.RaiseException(const ExceptClass: TExceptionClass; const ExceptText: StdString;
  const ATextPos: Integer);
var
  ExpectedPos, LinePos, HorizPos: Integer;
begin
  if ATextPos <> -1 then
    ExpectedPos := ATextPos
  else
    ExpectedPos := FTextPos;

  if ExpectedPos < 1 then
    ExpectedPos := 1;

  EstimateLineAndPosition(ExpectedPos, LinePos, HorizPos);

  raise ExceptClass.Create(Format(ExceptText, [LinePos, HorizPos, Copy(FSourceCode, ExpectedPos, 20)]));
end;

procedure THeaderParser.ExpectedException(const ExpectedText: StdString; const ATextPos: Integer);
var
  ExpectedPos, LinePos, HorizPos: Integer;
begin
  if ATextPos <> -1 then
    ExpectedPos := ATextPos
  else
    ExpectedPos := FTextPos;

  if ExpectedPos < 1 then
    ExpectedPos := 1;

  EstimateLineAndPosition(ExpectedPos, LinePos, HorizPos);

  raise EUnexpectedCode.Create(Format(SUnexpectedCode, [LinePos, HorizPos, ExpectedText,
    Copy(FSourceCode, ExpectedPos, 20)]));
end;

function THeaderParser.RetrieveIntegerValue: StdString;
var
  ValueType: TNumericalValueType;
begin
  ValueType := RetrieveNumericalValue(Result);
  if not (ValueType in [TNumericalValueType.Integer, TNumericalValueType.Hex]) then
    Result := '';
end;

function THeaderParser.ProcessIdentifier: StdString;
begin
  Result := RetrieveIdentifier;
  if Length(Result) < 1 then
    RaiseException(EIdentifierExpected, SIdentifierExpected);
end;

function THeaderParser.ProcessNumericalValue: StdString;
var
  ValueType: TNumericalValueType;
begin
  ValueType := RetrieveNumericalValue(Result);
  if (ValueType = TNumericalValueType.None) or (Length(Result) < 1) then
    RaiseException(ENumericalValueExpected, SNumericalValueExpected);
end;

function THeaderParser.ProcessIntegerValue: StdString;
var
  ValueType: TNumericalValueType;
begin
  ValueType := RetrieveNumericalValue(Result);
  if (not (ValueType in [TNumericalValueType.Integer, TNumericalValueType.Hex])) or (Length(Result) < 1) then
    RaiseException(EIntegerValueExpected, SIntegerValueExpected);
end;

function THeaderParser.ProcessGuidIntegerValue(const MinLetterCount: Integer): StdString;
begin
  Result := ProcessIntegerValue;

  if (Length(Result) > 1) and (Result[1] = '$') then
    Delete(Result, 1, 1);

  while Length(Result) < MinLetterCount do
    Result := '0' + Result;
end;

function THeaderParser.ProcessNumericalValueOrIdentifier: StdString;
var
  ValueType: TNumericalValueType;
begin
  ValueType := RetrieveNumericalValue(Result);

  if (ValueType = TNumericalValueType.None) or (Length(Result) < 1) then
  begin
    Result := RetrieveIdentifier;
    if Length(Result) < 1 then
      RaiseException(ENumericalValueOrIdentifierExpected, SNumericalValueOrIdentifierExpected);
  end;
end;

procedure THeaderParser.ProcessExpectedChar(const Ch: StdChar);
begin
  if not AdvanceWithChar(Ch) then
    ExpectedException(Ch);
end;

function THeaderParser.ProcessGUIDValue: StdString;
begin
  Result := RetrieveGUIDValue;
  if Length(Result) < 1 then
    RaiseException(EGUIDValueExpected, SGUIDValueExpected);
end;

function THeaderParser.RetrieveLogicalOperator: TLogicalOperator;
begin
  // && operator
  if AdvanceWithText('&&') then
    Exit(TLogicalOperator.C_AND);

  // || operator
  if AdvanceWithText('||') then
    Exit(TLogicalOperator.C_OR);

  Result := TLogicalOperator.None;
end;

function THeaderParser.RetrieveComparisonOperator: TComparisonOperator;
begin
  // == operator
  if AdvanceWithText('==') then
    Exit(TComparisonOperator.Equal);

  // != operator
  if AdvanceWithText('!=') then
    Exit(TComparisonOperator.NotEqual);

  // >= operator
  if AdvanceWithText('>=') then
    Exit(TComparisonOperator.BiggerOrEqual);

  // <= operator
  if AdvanceWithText('<=') then
    Exit(TComparisonOperator.SmallerOrEqual);

  // > operator
  if AdvanceWithChar('>') then
    Exit(TComparisonOperator.Bigger);

  // < operator
  if AdvanceWithChar('<') then
    Exit(TComparisonOperator.Smaller);

  Result := TComparisonOperator.None;
end;

function THeaderParser.RetrieveExpressionOperator: StdString;
begin
  // << operator
  if AdvanceWithText('<<') then
    Exit('shl');

  // >> operator
  if AdvanceWithText('>>') then
    Exit('shr');

  // & operator
  if AdvanceWithChar('&') then
    Exit('and');

  // | operator
  if AdvanceWithChar('|') then
    Exit('or');

  // + operator
  if AdvanceWithChar('+') then
    Exit('+');

  // - operator
  if AdvanceWithChar('-') then
    Exit('+');

  // * operator
  if AdvanceWithChar('*') then
    Exit('*');

  // / operator
  if AdvanceWithChar('/') then
    Exit('div');

  Result := '';
end;

function THeaderParser.ProcessExpressionOperator: StdString;
begin
  Result := RetrieveExpressionOperator;

  if Length(Result) < 1 then
    RaiseException(EExpressionOperatorExpected, SExpressionOperatorExpected, TextPos);
end;

function THeaderParser.AddPointerToDataType(const Text: StdString): StdString;
begin
  if SameText(Text, 'TRect') then
    Result := 'PRect'
  else if SameText(Text, 'TPoint') then
    Result := 'PRect'
  else if SameText(Text, 'TSize') then
    Result := 'PSize'
  else if SameText(Text, 'TGuid') then
    Result := 'PGuid'
  else if SameText(Text, 'Pointer') then
    Result := 'Pointer'
  else
    Result := 'P' + Text;
end;

function THeaderParser.IsDataTypeComplex(const DataTypeName: StdString): Boolean;
var
  DataTypeInfo: TDataTypeInfo;
begin
  // If data type is not in database, likely it is not a fundamental type and therefore is structure.
  if not FDataTypes.Resolve(DataTypeName, DataTypeInfo) then
    Exit(True);

  Result := DataTypeInfo.Group in [TDataTypeGroup.Structure, TDataTypeGroup.&Interface, TDataTypeGroup.Callback];
end;

function THeaderParser.IsDataTypeInterfaceGuess(const DataTypeName: StdString): Boolean;
begin
  Result := False;

  if (TParserOption.GuessInterfaces in FOptions) and (Length(DataTypeName) > 2) and (DataTypeName[1] = 'I') then
    Result := True;
end;

function THeaderParser.SkipComments(const ALeaveComments: Boolean): Boolean;
const
  MultiLineCommentStart = '/*';
  MultiLineCommentEnd = '*/';
  SingleLineComment = '//';
var
  Text: StdString;
  EndTermFound: Boolean;
begin
  // Skip "/* ... */" comments.
  if AdvanceWithText(MultiLineCommentStart) then
  begin
    if ALeaveComments then
    begin
      StartCodeSection(TCodeSection.None);
      StartSection(FSection, TSubSection.Comments);

      AddText('{ ');

      repeat
        EndTermFound := RetrieveTextUntilTextInstanceOrEndOfLine(MultiLineCommentEnd, Text);

        if EndTermFound then
          AddLine(Text + ' }')
        else
          AddLine(Text);
      until EndTermFound;
    end
    else
      SkipAfterNextTextInstance(MultiLineCommentEnd);

    Exit(True)
  end;

  // Skip "//" comments.
  if AdvanceWithText(SingleLineComment) then
  begin
    if ALeaveComments then
    begin
      StartCodeSection(TCodeSection.None);
      StartSection(FSection, TSubSection.Comments);

      AddLine('//' + RetrieveTextUntilEndOfLine);
    end
    else
      SkipToNextLine;

    Exit(True)
  end;

  Result := False;
end;

function THeaderParser.SkipUnsupportedDirective(const Directive: StdString): Boolean;
begin
  Result := StartsWithText(Directive);
  if Result then
  begin
    if not (TParserOption.LeaveUnsupportedSymbols in FOptions) then
    begin
      Advance(Length(Directive));
      SkipToNextLine;
    end
    else
    begin
      StartCodeSection(TCodeSection.None);
      StartSection(TSection.UnsupportedSymbols);

      AddLine('// ' + RetrieveTextUntilEndOfLine);
    end;
  end;
end;

function THeaderParser.SkipUnsupportedDirectives: Boolean;
const
  DirectivePRAGMA = '#pragma';
  DirectiveINCLUDE = '#include';
  DirectiveERROR = '#error';
  DirectiveEXTERN = 'extern';
  DirectiveEXTERNC = 'EXTERN_C';
begin
  // #pragma
  if SkipUnsupportedDirective(DirectivePRAGMA) then
    Exit(True);

  // #include
  if SkipUnsupportedDirective(DirectiveINCLUDE) then
    Exit(True);

  // #error
  if SkipUnsupportedDirective(DirectiveERROR) then
    Exit(True);

  // extern
  if SkipUnsupportedDirective(DirectiveEXTERN) then
    Exit(True);

  // EXTERN_C
  if SkipUnsupportedDirective(DirectiveEXTERNC) then
    Exit(True);

  Result := False;
end;

procedure THeaderParser.ProcessConditionalIFDEF;
var
  Symbol: StdString;
begin
  Symbol := ProcessIdentifier;

  if (FDefineLevelSkip = -1) and (FDefineSymbols.IndexOf(Symbol) = -1) then
  begin
    AddDebugText('Skipping ' + Symbol + ' section.');
    FDefineLevelSkip := FDefineLevel;
  end;

  Inc(FDefineLevel);
end;

procedure THeaderParser.ProcessConditionalIFNDEF;
var
  Symbol: StdString;
begin
  Symbol := ProcessIdentifier;

  if (FDefineLevelSkip = -1) and (FDefineSymbols.IndexOf(Symbol) <> -1) then
  begin
    AddDebugText('Skipping ' + Symbol + ' section.');
    FDefineLevelSkip := FDefineLevel;
  end;

  Inc(FDefineLevel);
end;

procedure THeaderParser.ProcessConditionalENDIF;
begin
  Dec(FDefineLevel);
  if FDefineLevel < 0 then
    RaiseException(EMisplacedDirective, SMisplacedDirective, FTextPos - 6);

  // Ignore comment after ENDIF by skipping the rest of the line.
  SkipToNextLine;

  if FDefineLevelSkip = FDefineLevel then
  begin
    AddDebugText('Finished skipping section.');
    FDefineLevelSkip := -1;
  end;
end;

procedure THeaderParser.ProcessConditionalELSE;
begin
  Dec(FDefineLevel);
  if FDefineLevel < 0 then
    RaiseException(EMisplacedDirective, SMisplacedDirective, FTextPos - 6);

  // Ignore comment after ELSE by skipping the rest of the line.
  SkipToNextLine;

  if FDefineLevelSkip = FDefineLevel then
  begin
    AddDebugText('Finished skipping section, starting ELSE section.');
    FDefineLevelSkip := -1;
  end
  else if FDefineLevelSkip = -1 then
  begin
    AddDebugText('... and skipping this section.');
    FDefineLevelSkip := FDefineLevel;
  end;

  Inc(FDefineLevel);
end;

procedure THeaderParser.ProcessExpressionIFOperator(var FinalResult, NegateNextValue, ResultDefined: Boolean;
  CurrentValue: Boolean; var LastOperator: TLogicalOperator);
begin
  if (LastOperator <> TLogicalOperator.None) and (not ResultDefined) then
    RaiseException(EOperatorWithoutPredicate, SOperatorWithoutPredicate);

  if NegateNextValue then
  begin
    CurrentValue := not CurrentValue;
    NegateNextValue := False;
  end;

  case LastOperator of
    TLogicalOperator.None:
      FinalResult := CurrentValue;

    TLogicalOperator.C_OR:
      FinalResult := FinalResult or CurrentValue;

    TLogicalOperator.C_AND:
      FinalResult := FinalResult and CurrentValue;
  end;

  LastOperator := TLogicalOperator.None;
  ResultDefined := True;
end;

function THeaderParser.ProcessExpressionIF(const ParenthesisLevel: Integer): Boolean;
const
  FunctionDEFINED = 'defined';
  Function_WINAPI_FAMILY_PARTITION = 'WINAPI_FAMILY_PARTITION';
  Value_WINAPI_PARTITION_DESKTOP = 'WINAPI_PARTITION_DESKTOP';
  Value_WINAPI_PARTITION_APP = 'WINAPI_PARTITION_APP';
var
  ResultDefined, NegateNextValue: Boolean;
  LastOperator, NewOperator: TLogicalOperator;
  Comparison: TComparisonOperator;
  Symbol, ValueText: StdString;
  CurValue: Boolean;
  Index: Integer;
begin
  ResultDefined := False;
  NegateNextValue := False;
  LastOperator := TLogicalOperator.None;

  Result := False;

  while not IsEndOfFile do
  begin
    SkipWhitespace;

    // Enclosing ")"
    if AdvanceWithChar(')') then
    begin
      if (ParenthesisLevel <= 0) or (not ResultDefined) then
        RaiseException(EExpressionExpected, SExpressionExpected, FTextPos - 1);

      Exit;
    end;

    // (sub-expression)
    if AdvanceWithChar('(') then
    begin
      ProcessExpressionIFOperator(Result, NegateNextValue, ResultDefined, ProcessExpressionIF(ParenthesisLevel + 1),
        LastOperator);

      Continue;
    end;

    // Whitespace + EOL
    if EndsWithWhitespaceAndEndOfLine then
    begin
      if not ResultDefined then
        RaiseException(EExpressionExpected, SExpressionExpected);

      Exit;
    end;

    // ! operator
    if AdvanceWithChar('!') then
    begin
      if NegateNextValue then
        RaiseException(EExpressionExpected, SExpressionExpected, FTextPos - 1);

      NegateNextValue := True;
      Continue;
    end;

    // defined(symbol)
    if AdvanceWithText(FunctionDEFINED) then
    begin
      SkipWhitespace;

      ProcessExpectedChar('(');

      SkipWhitespace;
      Symbol := ProcessIdentifier;
      SkipWhitespace;

      ProcessExpectedChar(')');

      ProcessExpressionIFOperator(Result, NegateNextValue, ResultDefined, FDefineSymbols.IndexOf(Symbol) <> -1,
        LastOperator);

      Continue;
    end;

    // WINAPI_FAMILY_PARTITION(symbol)
    if AdvanceWithText(Function_WINAPI_FAMILY_PARTITION) then
    begin
      SkipWhitespace;

      ProcessExpectedChar('(');

      SkipWhitespace;
      Symbol := ProcessIdentifier;
      SkipWhitespace;

      ProcessExpectedChar(')');

      ProcessExpressionIFOperator(Result, NegateNextValue, ResultDefined, SameText(Symbol,
        Value_WINAPI_PARTITION_DESKTOP) or SameText(Symbol, Value_WINAPI_PARTITION_APP), LastOperator);

      Continue;
    end;

    // && or || operators
    NewOperator := RetrieveLogicalOperator;
    if NewOperator <> TLogicalOperator.None then
    begin
      if LastOperator <> TLogicalOperator.None then
        RaiseException(EOperatorWithoutPredicate, SOperatorWithoutPredicate);

      LastOperator := NewOperator;
      Continue;
    end;

    // [Symbol/Value] [Comparison] [Symbol/Value]
    Symbol := RetrieveIntegerValue;
    if Length(Symbol) < 1 then
    begin
      Symbol := ProcessIdentifier;

      Index := FDefineSymbols.IndexOf(Symbol);
      if Index <> -1 then
        Symbol := IntToStr(FDefineSymbols[Index].SymbolValue)
      else
        Symbol := '';
    end;

    SkipWhitespace;

    Comparison := RetrieveComparisonOperator;
    if Comparison = TComparisonOperator.None then
    begin
      if not EndsWithWhitespaceAndEndOfLine then
        RaiseException(EExpressionExpected, SExpressionExpected);

      // No comparison, just symbol
      if Length(Symbol) > 0 then
        CurValue := StrToInt(Symbol) <> 0
      else
        CurValue := False;

      ProcessExpressionIFOperator(Result, NegateNextValue, ResultDefined, CurValue, LastOperator);
      Continue;
    end;

    SkipWhitespace;

    // [Symbol/Value]
    ValueText := RetrieveIntegerValue;
    if Length(ValueText) < 1 then
    begin
      ValueText := ProcessIdentifier;

      Index := FDefineSymbols.IndexOf(ValueText);
      if Index <> -1 then
        ValueText := IntToStr(FDefineSymbols[Index].SymbolValue)
      else
        ValueText := '';
    end;

    CurValue := False;

    if (Length(Symbol) > 0) and (Length(ValueText) > 0) then
    begin
      case Comparison of
        TComparisonOperator.Equal:
          CurValue := StrToInt(Symbol) = StrToInt(ValueText);

        TComparisonOperator.NotEqual:
          CurValue := StrToInt(Symbol) <> StrToInt(ValueText);

        TComparisonOperator.Bigger:
          CurValue := StrToInt(Symbol) > StrToInt(ValueText);

        TComparisonOperator.Smaller:
          CurValue := StrToInt(Symbol) < StrToInt(ValueText);

        TComparisonOperator.BiggerOrEqual:
          CurValue := StrToInt(Symbol) >= StrToInt(ValueText);

        TComparisonOperator.SmallerOrEqual:
          CurValue := StrToInt(Symbol) <= StrToInt(ValueText);
      end;
    end;

    ProcessExpressionIFOperator(Result, NegateNextValue, ResultDefined, CurValue, LastOperator);
  end;
end;

procedure THeaderParser.ProcessConditionalIF;
begin
  if (FDefineLevelSkip = -1) and (not ProcessExpressionIF) then
  begin
    AddDebugText('Skipping IF section');
    FDefineLevelSkip := FDefineLevel;
  end;

  Inc(FDefineLevel);
end;

procedure THeaderParser.ProcessConditionalUNDEF;
var
  Symbol: StdString;
  Index: Integer;
begin
  Symbol := ProcessIdentifier;

  Index := FDefineSymbols.IndexOf(Symbol);
  if Index <> -1 then
    FDefineSymbols.Remove(Index);

  AddDebugText('Undefined symbol: ' + Symbol);
end;

function THeaderParser.ParseExpression(const DefineExpression: Boolean;
  const ParenthesisLevel: Integer): StdString;
var
  SubExpression, Identifier, ExOperator: StdString;
  AcceptOperator: Boolean;
begin
  Result := '';
  AcceptOperator := False;

  while not IsEndOfFile do
  begin
    SkipWhitespace;

    if SkipComments(False) then
      Continue;

    // '(' start of expression
    if AdvanceWithChar('(') then
    begin
      SubExpression := ParseExpression(DefineExpression, ParenthesisLevel + 1);

      if (ParenthesisLevel = 0) and (Length(Result) < 1) then
        Exit(SubExpression)
      else if Length(Result) > 0 then
        Result := Result + ' (' + SubExpression + ')'
      else
        Result := Result + '(' + SubExpression + ')';

      if Length(SubExpression) > 0 then
        AcceptOperator := True;

      Continue;
    end;

    // ')' end of expression
    if AdvanceWithChar(')') then
    begin
      if ParenthesisLevel < 1 then
        RaiseException(EExpressionExpected, SExpressionExpected);

      Exit;
    end;

    if not DefineExpression then
    begin
      SkipWhitespace;
      if StartsWithChar(',') or StartsWithChar('}') then
        Exit;
    end;

    SkipWhitespace;
    if StartsWithText('#endif') then
      Exit;

    // operator
    if AcceptOperator then
    begin
      ExOperator := RetrieveExpressionOperator;
      SkipWhitespace;

      if Length(ExOperator) < 1 then
        Exit;

      Result := Result + ' ' + ExOperator;
      AcceptOperator := False;
      Continue;
    end;

    // number or identifier
    Identifier := ProcessNumericalValueOrIdentifier;

    if Length(Result) > 0 then
      Result := Result + ' ' + Identifier
    else
      Result := Result + Identifier;

    AcceptOperator := True;

    if EndsWithWhitespaceAndEndOfLine then
      Exit;

    if not DefineExpression then
    begin
      SkipWhitespace;
      if StartsWithChar(',') or StartsWithChar('}') then
        Exit;
    end;
  end;
end;

procedure THeaderParser.ProcessConditionalDEFINE;
var
  Symbol, Value: StdString;
  ValueDefined: Boolean;
begin
  Symbol := ProcessIdentifier;
  ValueDefined := FDefineSymbols.IndexOf(Symbol) <> -1;

  // #define [SYMBOL]
  if EndsWithWhitespaceAndEndOfLine then
  begin
    if FIgnoreSymbols.IndexOf(Symbol) = -1 then
    begin
      FDefineSymbols.Define(Symbol);
      AddDebugText('Defined symbol: ' + Symbol);
    end;

    Exit;
  end;

  // Value defines usually have space between name and "(", while functions do not.
  Advance;

  Value := ParseExpression;
  if Length(Value) < 1 then
    RaiseException(EExpressionExpected, SExpressionExpected);

  if (FIgnoreSymbols.IndexOf(Symbol) = -1) and (not ValueDefined) then
  begin
    StartCodeSection(TCodeSection.Constants_Defines);
    StartSection(TSection.Constants);

    AddLine(SideSpaceText + Symbol + ' = ' + Value + ';');

    FDefineSymbols.Define(Symbol, StrToIntDef(Value, 0));
  end;
end;

function THeaderParser.ProcessConditionals: Boolean;
const
  DirectiveIF = '#if';
  DirectiveIFDEF = '#ifdef';
  DirectiveIFNDEF = '#ifndef';
  DirectiveENDIF = '#endif';
  DirectiveUNDEF = '#undef';
  DirectiveELSE = '#else';
  DirectiveDEFINE = '#define';
var
  SavedPos: Integer;
  UnsupportedDefine: Boolean;
begin
  // #ifdef
  if AdvanceWithText(DirectiveIFDEF) then
  begin
    SkipWhitespace;
    ProcessConditionalIFDEF;
    Exit(True);
  end;

  // #ifndef
  if AdvanceWithText(DirectiveIFNDEF) then
  begin
    SkipWhitespace;
    ProcessConditionalIFNDEF;
    Exit(True);
  end;

  // #endif
  if AdvanceWithText(DirectiveENDIF) then
  begin
    SkipWhitespace;
    ProcessConditionalENDIF;
    Exit(True);
  end;

  // #else
  if AdvanceWithText(DirectiveELSE) then
  begin
    SkipWhitespace;
    ProcessConditionalELSE;
    Exit(True);
  end;

  // #if
  if AdvanceWithText(DirectiveIF) then
  begin
    ProcessConditionalIF;
    Exit(True);
  end;

  // Inside IFDEFed section, skip code.
  if FDefineLevelSkip <> -1 then
  begin
    AddDebugText('Skipping (due to IFDEFs):' + RetrieveTextUntilEndOfLine);
    Exit(True);
  end;

  // #undef
  if AdvanceWithText(DirectiveUNDEF) then
  begin
    SkipWhitespace;
    ProcessConditionalUNDEF;
    Exit(True);
  end;

  // #define
  if AdvanceWithText(DirectiveDEFINE) then
  begin
    SkipWhitespace;

    SavedPos := FTextPos;
    UnsupportedDefine := False;

    try
      ProcessConditionalDEFINE;
    except
      UnsupportedDefine := True;
    end;

    if UnsupportedDefine then
    begin
      FTextPos := SavedPos;

      AddDebugText('Cannot interpret this define.');

      if TParserOption.LeaveUnsupportedSymbols in FOptions then
      begin
        StartCodeSection(TCodeSection.None);
        StartSection(TSection.UnsupportedSymbols);

        AddLine('// ' + RetrieveTextUntilEndOfLine);
      end
      else
        SkipToNextLine;
    end;

    Exit(True);
  end;

  Result := False;
end;

procedure THeaderParser.ProcessStatementTypedefINTERFACE(const IgnoreSecondPart: Boolean);
var
  ForwardName, ForwardType: StdString;
  PointerIndex, I: Integer;
begin
  ForwardName := ProcessIdentifier;
  SkipWhitespaceAndLineBreaks;

  if not IgnoreSecondPart then
  begin
    PointerIndex := ProcessPointerReferences;

    ForwardType := ProcessIdentifier;
    SkipWhitespaceAndLineBreaks;
  end
  else
    PointerIndex := 0;

  ProcessExpectedChar(';');

  if FIgnoreSymbols.IndexOf(ForwardName) = -1 then
  begin
    FDataTypes.Define(ForwardName, '', ForwardName, DefaultPointerDataTypeSize, 0, TDataTypeGroup.&Interface,
      TDataTypeRelation.&Forward);

    for I := 0 to PointerIndex - 1 do
      ForwardType := AddPointerToDataType(ForwardType);

    StartCodeSection(TCodeSection.Types_Forwards);
    StartSection(TSection.Types, TSubSection.Forwards);

    if PointerIndex > 0 then
      AddLine(SideSpaceText + ForwardName + ' = ' + ForwardType + ';')
    else
      AddLine(SideSpaceText + ForwardName + ' = interface;');
  end;
end;

procedure THeaderParser.ProcessStatementTypedefENUM;
var
  EnumName, VariableName, ConstName, ExpressionText, ValuesText: StdString;
  EnumIndex, PrevSideSpace: Integer;
  ShouldIgnore: Boolean;
begin
  EnumName := RetrieveIdentifier;//ProcessIdentifier;
  SkipWhitespaceAndLineBreaks;

  ValuesText := '';

  if AdvanceWithChar('{') then
  begin
    EnumIndex := 0;

    PrevSideSpace := FSideSpace;
    FSideSpace := DefaultTabWidth;
    try
      while not IsEndOfFile do
      begin
        SkipWhitespaceAndLineBreaks;

        if SkipComments(False) then
          Continue;

        if ProcessConditionals then
          Continue;

        ConstName := RetrieveIdentifier;
        if Length(ConstName) < 1 then
          Break;

        SkipWhitespaceAndLineBreaks;

        if AdvanceWithChar('=') then
        begin
          SkipWhitespaceAndLineBreaks;

          ExpressionText := ParseExpression(False);
          if Length(ExpressionText) < 1 then
            RaiseException(EExpressionExpected, SExpressionExpected);

          SkipWhitespaceAndLineBreaks;

          EnumIndex := StrToIntDef(ExpressionText, EnumIndex) + 1;
          ValuesText := ValuesText + SideSpaceText + ConstName + ' = ' + ExpressionText + ';' + SLineBreak;
        end
        else
        begin
          ValuesText := ValuesText + SideSpaceText + ConstName + ' = ' + IntToStr(EnumIndex) + ';' + SLineBreak;
          Inc(EnumIndex);
        end;

        if not AdvanceWithChar(',') then
          Break;
      end;
    finally
      FSideSpace := PrevSideSpace;
    end;

    while SkipComments(False) do ;

    SkipWhitespaceAndLineBreaks;
    ProcessExpectedChar('}');
    SkipWhitespaceAndLineBreaks;
  end
  else
    RaiseException(EEnumerationExpected, SEnumerationExpected);

  // Enumeration Variable Name
  VariableName := ProcessIdentifier;
  SkipWhitespaceAndLineBreaks;

  ShouldIgnore := FIgnoreSymbols.IndexOf(EnumName)  <> -1;

  if (Length(VariableName) > 0) and (not SameText(EnumName, VariableName)) then
  begin
    ShouldIgnore := ShouldIgnore or (FIgnoreSymbols.IndexOf(VariableName) <> -1);

    if not ShouldIgnore then
      FDataTypeAliases.Define(EnumName, VariableName, 0, TDataTypeAliasType.Inverse);

    EnumName := VariableName;
  end;

  // Are there additional aliases?
  if AdvanceWithChar(',') then
    ProcessRemainingAliases(EnumName);

  // Skip ", ... other declarations"
  ProcessExpectedChar(';');

  if not ShouldIgnore then
  begin
    StartCodeSection(TCodeSection.Type_Enums, EnumName);
    StartSection(TSection.Types, TSubSection.Enums);

    AddLine(SideSpaceText + 'P' + EnumName + ' = ^' + EnumName + ';');
    AddLine(SideSpaceText + EnumName + ' = LongWord;');

    FDataTypes.Define(EnumName, '', EnumName, 4, 0, TDataTypeGroup.Enum, TDataTypeRelation.Declared);

    if Length(ValuesText) > 0 then
    begin
      StartCodeSection(TCodeSection.Constants_Enums, EnumName);
      StartSection(TSection.Constants, TSubSection.Enums);

      AddText(ValuesText);
    end;
  end;
end;

function THeaderParser.ProcessMultiWordDataType(const FirstWord: StdString): StdString;
var
  I, WordCount, InitialPos: Integer;
  FullText, CurText: StdString;
begin
  InitialPos := FTextPos;

  for WordCount := 3 downto 1 do
  begin
    FTextPos := InitialPos;
    FullText := FirstWord;

    for I := 0 to WordCount - 1 do
    begin
      SkipWhitespaceAndLineBreaks;
      CurText := RetrieveIdentifier;

      if Length(CurText) < 1 then
      begin
        FullText := '';
        Break;
      end;

      FullText := FullText + ' ' + CurText;
    end;

    if Length(FullText) > 0 then
    begin
      if FDataTypes.Exists(FullText) then
        Exit(FullText);
    end;
  end;

  FTextPos := InitialPos;
  Result := FirstWord;
end;

function THeaderParser.TranslateDataType(const DataType, Translated: StdString; const PointerIndex,
  DereferenceCount: Integer): StdString;
const
  DataTypeRECT = 'RECT';
  DataTypePOINT = 'POINT';
  DataTypeSIZE = 'SIZE';
  DataTypeGUID = 'GUID';
  DataTypeVOID = 'VOID';
begin
  if SameText(DataType, DataTypeRECT) and (DereferenceCount > 0) then
    Result := 'PRect'
  else if SameText(DataType, DataTypePOINT) and (DereferenceCount > 0) then
    Result := 'PPoint'
  else if SameText(DataType, DataTypeGUID) and (DereferenceCount > 0) then
    Result := 'PGuid'
  else if SameText(DataType, DataTypeSIZE) and (DereferenceCount > 0) then
    Result := 'PSize'
  else if SameText(DataType, DataTypeVOID) and (DereferenceCount > 0) then
    Result := 'Pointer'
  else if SameText(DataType, DataTypeVOID) and (PointerIndex > 0) then
    Result := 'Pointer'
  else if (Length(DataType) > 0) and (DataType[1] = '_') and (PointerIndex = 1) then
  begin
    Result := DataType;
    Delete(Result, 1, 1);
    Result := Result + 'Rec';
  end
  else if (Length(DataType) > 0) and (SameText(DataType, Translated) or SameText(DataType, 'Color')) and
    (UpCase(DataType[1]) <> 'T') and (PointerIndex = 0) then
    Result := 'T' + Translated
  else
    Result := Translated;
end;

function THeaderParser.GetAnnotationInfo(const Text: StdString; out Info: TAnnotationInfo): Boolean;
var
  TextPos: Integer;
  Identifier: StdString;
begin
  Info.Name := '';
  Info.Access := TAnnotationAccess.None;
  Info.Multiple := False;
  Info.Optional := False;

  if Length(Text) < 3 then
    Exit(False);

  if (Text[1] = '_') and (Text[2] = '_')  then
    TextPos := 3 // __RPC
  else if (Text[1] = '_') and (Text[Length(Text)] = '_') then
    TextPos := 2 // _normal_
  else
    Exit(False);

  Result := True;

  while TextPos <= Length(Text) do
  begin
    AnnotationSkipWhitespace(Text, TextPos);
    if TextPos > Length(Text) then
      Break;

    Identifier := AnnotationRetrieveIdentifier(Text, TextPos);
    if Length(Identifier) < 1 then
      Break;

    if SameText(Identifier, 'opt') then
    begin
      Info.Optional := True;
      Continue;
    end;

    if SameText(Identifier, 'in') or SameText(Identifier, 'inptr') or SameText(Identifier, 'inref') then
    begin
      Info.Access := TAnnotationAccess.Read;
      Continue;
    end;

    if SameText(Identifier, 'out') or SameText(Identifier, 'outptr') or SameText(Identifier, 'outref') or
      SameText(Identifier, 'ret') or SameText(Identifier, 'result') then
    begin
      Info.Access := TAnnotationAccess.Write;
      Continue;
    end;

    if SameText(Identifier, 'inout') or SameText(Identifier, 'inoutptr') then
    begin
      Info.Access := TAnnotationAccess.ReadWrite;
      Continue;
    end;

    if SameText(Identifier, 'reads') or SameText(Identifier, 'writes') or SameText(Identifier, 'updates') or
      SameText(Identifier, 'bytes') then
    begin
      Info.Multiple := True;
      Continue;
    end;
  end;

  Info.Name := Text;
end;

function THeaderParser.IsAnnotation(const Text: StdString): Boolean;
var
  Info: TAnnotationInfo;
begin
  Result := GetAnnotationInfo(Text, Info);
end;

procedure THeaderParser.DereferencePointer(var PointerIndex, DereferenceCount: Integer);
begin
  if PointerIndex > 0 then
  begin
    Dec(PointerIndex);
    Inc(DereferenceCount);
  end;
end;

function THeaderParser.ProcessPointerReferences: Integer;
begin
  Result := 0;

  while AdvanceWithChar('*') do
    Inc(Result);

  if Result > 0 then
    SkipWhitespaceAndLineBreaks;
end;

procedure THeaderParser.UpdateParameterName(var ParameterName: StdString; const DataTypeName,
  TranslatedDataType: StdString; const PointerIndex, DereferenceCount: Integer; const ExistingParameters: TStrings);
var
  I: Integer;
begin
  if TParserOption.ShortenParameterNames in FOptions then
  begin
    // Remove extra "p" prefixes.
{    for I := 0 to (PointerIndex + DereferenceCount) - 1 do
      if (Length(ParameterName) > 1) and (ParameterName[1] = 'p') and
      (not SameText(ParameterName, 'program')) then
        Delete(ParameterName, 1, 1)
      else
        Break;}

    if SameText(DataTypeName, 'DWORD') and (Length(ParameterName) > 2) and (ParameterName[1] = 'd') and
      (ParameterName[2] = 'w') then
      Delete(ParameterName, 1, 2);

    // If parameter has all lower case letters, make sure at least one letter has upper case.
    if Length(ParameterName) > 0 then
      ParameterName[1] := UpCase(ParameterName[1]);
  end;

  // Make sure the parameter name doesn't conflict with any other names.
  while IsReservedWord(ParameterName) or ((ExistingParameters <> nil) and
    (ExistingParameters.IndexOf(ParameterName) <> -1)) or SameText(ParameterName, TranslatedDataType) do
    ParameterName := '_' + ParameterName;
end;

procedure THeaderParser.ProcessStructUnionVariableName;
const
  StatementSemicolon = ';';
begin
  SkipWhitespaceAndLineBreaks;

  // No identifier, just ";".
  if AdvanceWithChar(StatementSemicolon) then
    Exit;

  // Name identifier.
  ProcessIdentifier;
  SkipWhitespaceAndLineBreaks;

  // Possibly '(' and ')'.
  ProcessSkipOpenCloseBrackets;

  // Forcefully expect ';'
  ProcessExpectedChar(StatementSemicolon);
end;

function THeaderParser.ProcessStructParameters(const BaseUnion: TUnionParameter;
  const BaseStruct: TStructParameter): Integer;
const
  StructKeywordUNION = 'union';
  StructKeywordCONST = 'const';
  StructKeywordSTRUCT = 'struct';
var
  DataTypeName: StdString;
  DataTypeOffset, DeclarationPosition: Integer;
  NormalParameter: TNormalParameter;
  UnionParameter: TUnionParameter;
  StructParameter: TStructParameter;
  UnsupportedStruct, AnotherDeclaration: Boolean;
  Declaration: TParameterDeclaration;
begin
  Result := 0;

  if (BaseUnion = nil) and (BaseStruct = nil) then
  begin
    FParameters.Clear;
    FParameterNames.Clear;
  end;

  if not AdvanceWithChar('{') then
    Exit;

  if BaseUnion <> nil then
    DataTypeOffset := BaseUnion.Offset
  else if BaseStruct <> nil then
    DataTypeOffset := BaseStruct.Offset
  else
    DataTypeOffset := 0;

  UnsupportedStruct := False;
  AnotherDeclaration := False;

  while not IsEndOfFile do
  begin
    SkipWhitespaceAndLineBreaks;

    if SkipComments(False) then
      Continue;

    if ProcessConditionals then
      Continue;

    if AnotherDeclaration then
    begin
      Declaration.Name := ProcessIdentifier;
      SkipWhitespaceAndLineBreaks;
    end
    else
    begin
      DeclarationPosition := FTextPos;

      // Data Type
      DataTypeName := RetrieveIdentifier;
      if Length(DataTypeName) < 1 then
        Break;

      // 'union'
      if SameText(DataTypeName, StructKeywordUNION) then
      begin
        SkipWhitespaceAndLineBreaks;

        UnionParameter := TUnionParameter.Create(DataTypeOffset);

        if BaseUnion <> nil then
          BaseUnion.Add(UnionParameter)
        else if BaseStruct <> nil then
          BaseStruct.Add(UnionParameter)
        else
          FParameters.Add(UnionParameter);

        if ProcessStructParameters(UnionParameter) = -1 then
          UnsupportedStruct := True;

        if BaseUnion = nil then
          Inc(DataTypeOffset, UnionParameter.Size);

        // Skip union name, if such is present.
        ProcessStructUnionVariableName;
        Continue;
      end;

      // 'const'
      if SameText(DataTypeName, StructKeywordCONST) then
      begin
        SkipWhitespaceAndLineBreaks;
        DataTypeName := ProcessIdentifier;
      end;

      // 'struct'
      if SameText(DataTypeName, StructKeywordSTRUCT) then
      begin
        SkipWhitespaceAndLineBreaks;

        StructParameter := TStructParameter.Create(DataTypeOffset);

        if BaseUnion <> nil then
          BaseUnion.Add(StructParameter)
        else if BaseStruct <> nil then
          BaseStruct.Add(StructParameter)
        else
          FParameters.Add(StructParameter);

        if ProcessStructParameters(nil, StructParameter) = -1 then
          UnsupportedStruct := True;

        if BaseUnion = nil then
          Inc(DataTypeOffset, StructParameter.Size);

        // Skip struct name, if such is present.
        ProcessStructUnionVariableName;
        Continue;
      end;

      FTextPos := DeclarationPosition;
    end;

    if AnotherDeclaration or ProcessMethodParameterDeclaration(nil, Declaration) then
    begin
      ConvertParameterDeclarationIntoStructParameter(Declaration, DataTypeOffset, NormalParameter);

      if BaseUnion <> nil then
        BaseUnion.Add(NormalParameter)
      else if BaseStruct <> nil then
        BaseStruct.Add(NormalParameter)
      else
        FParameters.Add(NormalParameter);

      if FParameterNames.IndexOf(NormalParameter.Name) = -1 then
        FParameterNames.Add(NormalParameter.Name);

      if FParameterNames.IndexOf(NormalParameter.TranslatedType) = -1 then
        FParameterNames.Add(NormalParameter.TranslatedType);
    end
    else
    begin
      UnsupportedStruct := True;
      NormalParameter := nil;
    end;

    if (BaseUnion = nil) and (NormalParameter <> nil) then
      Inc(DataTypeOffset, NormalParameter.Size);

    AnotherDeclaration := False;

    if AdvanceWithChar(',') then
    begin
      SkipWhitespaceAndLineBreaks;
      AnotherDeclaration := True;
      Continue;
    end;
    ProcessExpectedChar(';');
  end;

  SkipWhitespaceAndLineBreaks;
  ProcessExpectedChar('}');

  if UnsupportedStruct then
    Result := -1
  else
    Result := DataTypeOffset;
end;

procedure THeaderParser.ProcessDeclarationsIntoRecordEntries(const BaseUnion: TUnionParameter;
  const BaseStruct: TStructParameter);
var
  InputParameterIndex, I: Integer;
  Parameter, HolderParam: TCustomParameter;
  HolderItem, ParameterHolder, SuitableItem: TParameterHolder;
begin
  if (BaseUnion = nil) and (BaseStruct = nil) then
  begin
    FParameterHolderList.Clear;
    FParameterHolderListRoot.Clear;
  end;

  InputParameterIndex := 0;

  while ((BaseUnion = nil) and (BaseStruct = nil) and (InputParameterIndex < FParameters.Count)) or
    ((BaseUnion <> nil) and (BaseStruct = nil) and (InputParameterIndex < BaseUnion.Count)) or
    ((BaseUnion = nil) and (BaseStruct <> nil) and (InputParameterIndex < BaseStruct.Count)) do
  begin
    if BaseUnion <> nil then
      Parameter := BaseUnion[InputParameterIndex]
    else if BaseStruct <> nil then
      Parameter := BaseStruct[InputParameterIndex]
    else
      Parameter := FParameters[InputParameterIndex];

    if Parameter is TNormalParameter then
    begin
      ParameterHolder := TParameterHolder.Create(Parameter, False);

      SuitableItem := nil;

      for I := 0 to FParameterHolderList.ChildCount - 1 do
      begin
        HolderItem := FParameterHolderList[I];
        if HolderItem = nil then
          Continue;

        HolderParam := HolderItem.Parameter;
        if HolderParam = nil then
          Continue;

        if Parameter.Offset = HolderParam.Offset + HolderParam.Size then
          if (SuitableItem = nil) or (SuitableItem.ChildCount > HolderItem.ChildCount) then
            SuitableItem := HolderItem;
      end;

      if SuitableItem <> nil then
        SuitableItem.Add(ParameterHolder)
      else
        FParameterHolderListRoot.Add(ParameterHolder);

      FParameterHolderList.Add(ParameterHolder);
    end
    else if Parameter is TUnionParameter then
      ProcessDeclarationsIntoRecordEntries(TUnionParameter(Parameter))
    else if Parameter is TStructParameter then
      ProcessDeclarationsIntoRecordEntries(nil, TStructParameter(Parameter));

    Inc(InputParameterIndex);
  end;
end;

function THeaderParser.GetRecordEntriesText(const BaseNode: TParameterHolder): StdString;
var
  Node, Child: TParameterHolder;
  Parameter: TNormalParameter;
  DataTypeName: StdString;
  I: Integer;
begin
  Result := '';

  if BaseNode.ChildCount > 1 then
  begin
    Result := Result + SideSpaceText + 'case Integer of' + SLineBreak;

    Inc(FSideSpace, DefaultTabWidth);
    try
      for I := 0 to BaseNode.ChildCount - 1 do
      begin
        Child := BaseNode.Child[I];
        if Child = nil then
          Continue;

        Result := Result + SideSpaceText + IntToStr(I) + ':';

        Inc(FSideSpace, DefaultTabWidth);
        try
          Result := Result + SLineBreak + SideSpaceText + '(' + SLineBreak;

          Inc(FSideSpace, DefaultTabWidth);
          try
            Result := Result + GetRecordEntriesText(Child);
          finally
            Dec(FSideSpace, DefaultTabWidth);
          end;
          Result := Result + SideSpaceText + ');' + SLineBreak;
        finally
          Dec(FSideSpace, DefaultTabWidth);
        end;
      end;
    finally
      Dec(FSideSpace, DefaultTabWidth);
    end;
  end
  else
  begin
    Node := BaseNode;

    while Node <> nil do
    begin
      if Node.Parameter is TNormalParameter then
      begin
        Parameter := TNormalParameter(Node.Parameter);

        DataTypeName := Parameter.TranslatedType;

        for I := 0 to Parameter.PointerIndex - 1 do
          DataTypeName := AddPointerToDataType(DataTypeName);

        if TParserDebugOption.StructParamSizes in FDebugOptions then
          DataTypeName := DataTypeName + ' {Size: ' + IntToStr(Parameter.Size) + ', Offset: ' + IntToStr(Parameter.Offset) + '}';

        if Parameter.ArrayCount > 0 then
        begin
          Result := Result + SideSpaceText + Parameter.Name + ': array';

          for I := 0 to Parameter.ArrayCount - 1 do
          begin
            if I = 0 then
              Result := Result + '[0..'
            else
              Result := Result + ', 0..';

            try
              Result := Result + IntToStr(StrToInt(Parameter.Arrays[I]) - 1);
            except
              Result := Result + Parameter.Arrays[I] + ' - 1';
            end;
          end;

          Result := Result + '] of ' + DataTypeName + ';' + SLineBreak;
        end
        else
          Result := Result + SideSpaceText + Parameter.Name + ': ' + DataTypeName + ';' + SLineBreak;
      end;

      if Node.ChildCount > 1 then
      begin
        Result := Result + GetRecordEntriesText(Node);
        Break;
      end
      else if Node.ChildCount > 0 then
        Node := Node.Child[0]
      else
        Break;
    end;
  end;
end;

procedure THeaderParser.SkipRemainingStatementSTRUCT;
const
  StructOpenBracket = '{';
  StructCloseBracket = '}';
  StructSemicolon = ';';
var
  BracketLevel: Integer;
begin
  BracketLevel := -1;

  while not IsEndOfFile do
  begin
    SkipWhitespaceAndLineBreaks;

    if SkipComments(False) then
      Continue;

    if AdvanceWithChar(StructOpenBracket) then
    begin
      if BracketLevel = -1 then
        BracketLevel := 1
      else
        Inc(BracketLevel);

      Continue;
    end;

    if AdvanceWithChar(StructCloseBracket) then
    begin
      if BracketLevel <= 0 then
        RaiseException(EUnexpectedCode, SUnexpectedCode);

      Dec(BracketLevel);
      if BracketLevel <= 0 then
        Break;

      Continue;
    end;

    if (BracketLevel = -1) and StartsWithChar(StructSemicolon) then
      Break;

    // Skip the remaining characters.
    Advance;
  end;

  SkipWhitespaceAndLineBreaks;

  if not AdvanceWithChar(StructSemicolon) then
  begin
    RetrieveIdentifier;
    SkipWhitespaceAndLineBreaks;
    ProcessExpectedChar(StructSemicolon);
  end;
end;

procedure THeaderParser.ProcessRemainingAliases(const DataTypeName: StdString);
var
  Identifier: StdString;
  PointerIndex, AliasesDefined: Integer;
begin
  AliasesDefined := 0;

  while not IsEndOfFile do
  begin
    SkipWhitespaceAndLineBreaks;

    if AdvanceWithChar(',') then
    begin
      if AliasesDefined <= 0 then
        RaiseException(EIdentifierExpected, SIdentifierExpected);

      Continue;
    end;

    if StartsWithChar(';') then
    begin
      if AliasesDefined <= 0 then
        RaiseException(EIdentifierExpected, SIdentifierExpected);

      Exit;
    end;

    Identifier := RetrieveIdentifier;
    SkipWhitespaceAndLineBreaks;

    // 'FAR' keyword
    if SameText(Identifier, CommonDataTypeKeyword) then
      Identifier := '';

    if Length(Identifier) <= 0 then
    begin
      PointerIndex := ProcessPointerReferences;

      // Alias Name
      Identifier := ProcessIdentifier;
    end
    else
      PointerIndex := 0;

    FDataTypeAliases.Define(Identifier, DataTypeName, PointerIndex);
    Inc(AliasesDefined);
  end;
end;

procedure THeaderParser.ProcessStatementTypedefSTRUCT(const IsUnionFromStart: Boolean);
const
  StructModifierFAR = 'FAR';
var
  StructName, VariableName, RecordText: StdString;
  PointerIndex, StructSize: Integer;
  UnionParameter: TUnionParameter;
begin
  StructName := RetrieveIdentifier;// ProcessIdentifier;
  SkipWhitespaceAndLineBreaks;

  if FIgnoreSymbols.IndexOf(StructName) <> -1 then
  begin
    SkipRemainingStatementSTRUCT;
    Exit;
  end;

  if AdvanceWithChar(':') then
  begin
    AddDebugText('Skipping unsupported structure: ' + StructName);
    SkipRemainingStatementSTRUCT;
    Exit;
  end;

  RecordText := '';

  if IsUnionFromStart then
  begin
    FParameters.Clear;
    FParameterNames.Clear;

    UnionParameter := TUnionParameter.Create(0);
    FParameters.Add(UnionParameter);

    StructSize := ProcessStructParameters(UnionParameter);
  end
  else
    StructSize := ProcessStructParameters;

  if StructSize <> -1 then
  begin
    ProcessDeclarationsIntoRecordEntries;

    if FParameterHolderListRoot.ChildCount > 0 then
    begin
      StartCodeSection(TCodeSection.Types_Records, StructName);
      StartSection(TSection.Types, TSubSection.Structs);

      Inc(FSideSpace, DefaultTabWidth);
      try
        RecordText := GetRecordEntriesText(FParameterHolderListRoot);
      finally
        Dec(FSideSpace, DefaultTabWidth);
      end;
    end;
  end;

  SkipWhitespaceAndLineBreaks;

  if Length(RecordText) <= 0 then
  begin
    // Skip 'FAR' keyword.
    if AdvanceWithText(StructModifierFAR) then
      SkipWhitespaceAndLineBreaks;

    PointerIndex := ProcessPointerReferences;
  end
  else
    PointerIndex := 0;

  VariableName := RetrieveIdentifier;
  SkipWhitespaceAndLineBreaks;

  if (Length(RecordText) > 0) and (StructSize <> -1) then
  begin
    if (StructName <> VariableName) and (Length(VariableName) > 0) then
    begin
      FDataTypeAliases.Define(StructName, VariableName, 0, TDataTypeAliasType.Inverse);
      StructName := VariableName;
    end;

    AddLine(SideSpaceText + 'P' + StructName + ' = ^' + StructName + ';');

    AddLine(SideSpaceText + StructName + ' = record');
    AddText(RecordText);
    AddLine(SideSpaceText + 'end;');

    FDataTypes.Define(StructName, '', StructName, StructSize, PointerIndex, TDataTypeGroup.Structure,
      TDataTypeRelation.Declared);
  end
  else
  begin
{    if (Length(StructName) > 1) and (StructName[1] = 'I') and (Length(VariableName) > 3) and
      (VariableName[1] = 'L') and (VariableName[2] = 'P') and (PointerIndex = 1) and (StructSize <> -1) then
    begin // Special case, such as "IDirect3D * LPDirect3D"
      FDataTypes.Define(StructName, '', StructName, DefaultPointerDataTypeSize, 0, TDataTypeGroup.&Interface,
        TDataTypeRelation.&Forward);

      FDataTypes.Define(VariableName, StructName, StructName, DefaultPointerDataTypeSize, 1, TDataTypeGroup.&Interface,
        TDataTypeRelation.Redirect);

      AddDebugText('Skipping ' + VariableName + ' = ^' + StructName + ' statement.');
    end;}

    if (StructSize <> -1) and (Length(VariableName) > 0) and (not SameText(VariableName, StructName)) then
      FDataTypeAliases.Define(VariableName, StructName, PointerIndex)
    else
    begin
      if Length(VariableName) < 1 then
        VariableName := StructName;

      if StructSize <> -1 then
        AddDebugText('Skipping ' + VariableName + ' = record statement.')
      else
        AddDebugText('Skipping unsupported structure: ' + VariableName);
    end;
  end;

  // Are there additional aliases?
  if AdvanceWithChar(',') then
    ProcessRemainingAliases(StructName);

  // Skip remaining ", other names" structure.
  ProcessExpectedChar(';');
end;

procedure THeaderParser.ProcessStatementTypedefCALLBACK;
const
  CallingConventionSTDCALL = '__stdcall';
  CallingConventionWINAPI = 'WINAPI';
  CallingConventionAFTERWARP = 'AFTERWARP_API';
  StatementFAR = 'FAR';
var
  DataTypeName, ConventionsName, CallbackName, ParamText: StdString;
  PointerIndex, DereferenceCount, LLineWidth: Integer;
  DataTypeInfo: TDataTypeInfo;
begin
  // DataType or Access Modifier
  DataTypeName := ProcessIdentifier;
  SkipWhitespaceAndLineBreaks;

  // _annotations_
  if IsAnnotation(DataTypeName) then
  begin
    ProcessSkipOpenCloseBrackets;

    while SkipComments(False) do
      SkipWhitespaceAndLineBreaks;

    DataTypeName := ProcessIdentifier;
    SkipWhitespaceAndLineBreaks;
  end;

  // Resolve multiple-word data types.
  DataTypeName := ProcessMultiWordDataType(DataTypeName);

  ProcessExpectedChar('(');
  SkipWhitespaceAndLineBreaks;

  // __stdcall conventions
  ConventionsName := RetrieveIdentifier;
  SkipWhitespaceAndLineBreaks;

  if SameText(ConventionsName, StatementFAR) then
  begin
    ConventionsName := RetrieveIdentifier;
    SkipWhitespaceAndLineBreaks;
  end;

  // * pointer references
  PointerIndex := ProcessPointerReferences;

  CallbackName := ProcessIdentifier;
  SkipWhitespaceAndLineBreaks;

  ProcessExpectedChar(')');
  SkipWhitespaceAndLineBreaks;
  ProcessExpectedChar('(');

  Inc(FSideSpace, DefaultTabWidth);
  try
    ParamText := ProcessDeclarationMethodsParameters(LLineWidth, FLineWidth);
  finally
    Dec(FSideSpace);
  end;

  SkipWhitespaceAndLineBreaks;
  ProcessExpectedChar(';');

  if FIgnoreSymbols.IndexOf(CallbackName) = -1 then
  begin
    DereferenceCount := 0;

    if (PointerIndex > 0) and (Length(CallbackName) > 1) and (UpCase(CallbackName[1]) = 'P') then
    begin
      DereferencePointer(PointerIndex, DereferenceCount);
      Delete(CallbackName, 1, 1);
    end;

    FDataTypes.Define(CallbackName, '', CallbackName, DefaultPointerDataTypeSize, 0, TDataTypeGroup.Callback,
      TDataTypeRelation.Declared);

    if DereferenceCount > 0 then
      FDataTypes.Define('P' + CallbackName, '', 'P' + CallbackName, DefaultPointerDataTypeSize, 1,
        TDataTypeGroup.Pointer, TDataTypeRelation.Declared);

    if FDataTypes.GetInfo(DataTypeName, DataTypeInfo) then
      DataTypeName := TranslateDataType(DataTypeName, DataTypeInfo.Translated, PointerIndex, DereferenceCount)
    else
      DataTypeName := TranslateDataType(DataTypeName, DataTypeName, PointerIndex, DereferenceCount);

    StartCodeSection(TCodeSection.Types_Callbacks, CallbackName);
    StartSection(TSection.Types, TSubSection.Callbacks);

    if DereferenceCount > 0 then
      AddLine(SideSpaceText + 'P' + CallbackName + ' = ^' + CallbackName + ';');

    AddText(SideSpaceText + CallbackName + ' = function');

    AddText(ParamText);

    AddText(': ' + DataTypeName + ';');

    if SameText(ConventionsName, CallingConventionSTDCALL) or SameText(ConventionsName, CallingConventionWINAPI) then
      AddText(' stdcall;');

    if SameText(ConventionsName, CallingConventionAFTERWARP) then
      AddText(' cdecl;');

    AddLineBreak;
  end;
end;

procedure THeaderParser.ProcessStatementTYPEDEF;
const
  TypedefINTERFACE = 'interface';
  TypedefENUM = 'enum';
  TypedefSTRUCT = 'struct';
  TypedefUNION = 'union';
  StatementFAR = 'FAR';
var
  RedirectName, RedirectTypeName: StdString;
  DataTypeInfo: TDataTypeInfo;
  PointerIndex, I, SavedPos1, SavedPos2: Integer;
begin
  SkipWhitespaceAndLineBreaks;

  while SkipComments(False) do
    SkipWhitespaceAndLineBreaks;

  // interface
  if AdvanceWithText(TypedefINTERFACE) then
  begin
    SkipWhitespaceAndLineBreaks;
    ProcessStatementTypedefINTERFACE;
    Exit;
  end;

  // enum
  if AdvanceWithText(TypedefENUM) then
  begin
    SkipWhitespaceAndLineBreaks;
    ProcessStatementTypedefENUM;
    Exit;
  end;

  // struct
  if AdvanceWithText(TypedefSTRUCT) then
  begin
    SkipWhitespaceAndLineBreaks;
    ProcessStatementTypedefSTRUCT;
    Exit;
  end;

  // union
  if AdvanceWithText(TypedefUNION) then
  begin
    SkipWhitespaceAndLineBreaks;
    ProcessStatementTypedefSTRUCT(True);
    Exit;
  end;

  // [identifier]
  SavedPos1 := FTextPos;

  RedirectTypeName := ProcessIdentifier;
  SkipWhitespaceAndLineBreaks;

  SavedPos2 := FTextPos;

  // Callback declaration?
  if IsAnnotation(RedirectTypeName) or FDataTypes.Exists(RedirectTypeName) then
  begin
    FTextPos := SavedPos1;

    try
      ProcessStatementTypedefCALLBACK;
      Exit;
    except
      // Ignore exceptions, continue parsing rest of statement.
    end;

    FTextPos := SavedPos2;
  end;

  // Skip unusual 'FAR' statement.
  if AdvanceWithText(StatementFAR) then
    SkipWhitespaceAndLineBreaks;

  PointerIndex := ProcessPointerReferences;

  RedirectName := ProcessIdentifier;
  SkipWhitespaceAndLineBreaks;

  if AdvanceWithChar(',') then
    ProcessRemainingAliases(RedirectName);

  ProcessExpectedChar(';');

  if FIgnoreSymbols.IndexOf(RedirectName) = -1 then
  begin
    if (PointerIndex <= 0) and FDataTypes.GetInfo(RedirectTypeName, DataTypeInfo) then
    begin
      FDataTypes.Define(RedirectName, RedirectTypeName, RedirectName, DataTypeInfo.Size, DataTypeInfo.PointerIndex +
        PointerIndex, DataTypeInfo.Group, TDataTypeRelation.Redirect);
    end
    else
      FDataTypes.Define(RedirectName, '', RedirectName, DefaultPointerDataTypeSize, PointerIndex,
        TDataTypeGroup.Pointer, TDataTypeRelation.Declared);

    StartCodeSection(TCodeSection.Types_Redirects, RedirectName);
    StartSection(TSection.Types, TSubSection.Redirects);

    if PointerIndex <= 0 then
      AddLine(SideSpaceText + 'P' + RedirectName + ' = ^' + RedirectName + ';');

    if FDataTypes.GetInfo(RedirectTypeName, DataTypeInfo) then
      RedirectTypeName := TranslateDataType(RedirectTypeName, DataTypeInfo.Translated, PointerIndex, 0)
    else
      RedirectTypeName := TranslateDataType(RedirectTypeName, RedirectTypeName, PointerIndex, 0);

    for I := 0 to PointerIndex - 1 do
      RedirectTypeName := AddPointerToDataType(RedirectTypeName);

    AddLine(SideSpaceText + RedirectName + ' = ' + RedirectTypeName + ';');
  end;
end;

procedure THeaderParser.ProcessSkipOpenCloseBrackets(const StartsWithOpen: Boolean; const OpenBracket,
  CloseBracket: StdChar);
var
  BracketLevel: Integer;
begin
  if (not StartsWithOpen) or AdvanceWithChar(OpenBracket) then
  begin
    if StartsWithOpen then
      BracketLevel := 1
    else
      BracketLevel := 0;

    while not IsEndOfFile do
    begin
      if AdvanceWithChar(OpenBracket) then
      begin
        Inc(BracketLevel);
        Continue;
      end;

      if AdvanceWithChar(CloseBracket) then
      begin
        Dec(BracketLevel);
        if BracketLevel <= 0 then
        begin
          SkipWhitespaceAndLineBreaks;
          Break;
        end;

        Continue;
      end;

      Advance;
    end;
  end;
end;

function THeaderParser.MethodDeclarationEndsWidthVoid: Boolean;
var
  InitialPos: Integer;
  Identifier: StdString;
begin
  Result := False;

  InitialPos := FTextPos;

  Identifier := RetrieveIdentifier;
  if Length(Identifier) <= 0 then
    Exit;

  SkipWhitespaceAndLineBreaks;

  if SameText(Identifier, CommonDataTypeVOID) and AdvanceWithChar(')') then
  begin
    SkipWhitespaceAndLineBreaks;
    Exit(True);
  end;

  FTextPos := InitialPos;
end;

function THeaderParser.ProcessMethodParameterDeclaration(GenericParameterNameIndex: PInteger;
  out Declaration: TParameterDeclaration): Boolean;
const
  StatementCONST = 'const';
  StatementUNION = 'union';
  StatementENUM = 'enum';
  StatementINTERFACE = 'interface';
  StatementFAR = 'FAR';
  GenericParameterName = 'Param';
var
  Identifier: StdString;
  InitialPos: Integer;
begin
  Declaration.Reset;

  InitialPos := FTextPos;

  Identifier := ProcessIdentifier;
  SkipWhitespaceAndLineBreaks;

  // 'union' - not supported.
  if SameText(Identifier, StatementUNION) then
  begin
    FTextPos := InitialPos;
    Exit(False);
  end;

  // _annotations_
  if IsAnnotation(Identifier) then
  begin
    ProcessSkipOpenCloseBrackets;

    while SkipComments(False) do
      SkipWhitespaceAndLineBreaks;

    Declaration.Annotation := Identifier;

    Identifier := ProcessIdentifier;
    SkipWhitespaceAndLineBreaks;
  end;

  // 'enum' or 'interface' keywords
  if SameText(Identifier, StatementENUM) or SameText(Identifier, StatementINTERFACE) then
  begin
    // skip this weird declaration.
    Identifier := ProcessIdentifier;
    SkipWhitespaceAndLineBreaks;
  end;

  // additional keyword 'far'
  if AdvanceWithText(StatementFAR) then
    SkipWhitespaceAndLineBreaks;

  // Resolve multiple-word data types.
  Identifier := ProcessMultiWordDataType(Identifier);
  SkipWhitespaceAndLineBreaks;

  // '*' pointer references before CONST
  Declaration.PointerCount := ProcessPointerReferences;

  // 'const' modifier
  if SameText(Identifier, StatementCONST) then
  begin
    // Force "read-only" annotation if none exists, but "const" is present.
    if (Length(Declaration.Annotation) <= 0) and (Declaration.PointerCount <= 0) then
      Declaration.Annotation := '_In_';

    Identifier := ProcessIdentifier;
    SkipWhitespaceAndLineBreaks;
  end
  else if AdvanceWithText(StatementCONST) then
    SkipWhitespaceAndLineBreaks;

  // '*' pointer references after CONST
  Inc(Declaration.PointerCount, ProcessPointerReferences);

  Declaration.DataType := Identifier;

  // Parameter Name
  if StartsWithChar(',') or StartsWithChar(')') then
    // unnamed parameter, give it generic name
  begin
    if GenericParameterNameIndex = nil then
      RaiseException(EIdentifierExpected, SIdentifierExpected);

    Declaration.Name := GenericParameterName + IntToStr(GenericParameterNameIndex^);
    Inc(GenericParameterNameIndex^);
  end
  else
  begin
    Declaration.Name := ProcessIdentifier;
    SkipWhitespaceAndLineBreaks;
  end;

  // Resolve aliases.
  FDataTypeAliases.Resolve(Declaration.DataType, Declaration.PointerCount);

  // Process Arrays
  while AdvanceWithChar('[') do
  begin
    SkipWhitespace;
    Declaration.AddArraySize(ProcessNumericalValueOrIdentifier);

    SkipWhitespace;
    ProcessExpectedChar(']');
    SkipWhitespaceAndLineBreaks;
  end;

  // Bit Packed?
  if AdvanceWithChar(':') then
  begin
    SkipWhitespace;

    SkipWhitespaceAndLineBreaks;
    ProcessIntegerValue;
    SkipWhitespaceAndLineBreaks;

    // Unsupported struct
    Exit(False);
  end;

  Result := True;
end;

procedure THeaderParser.ConvertParameterDeclarationIntoMethodParameter(const Declaration: TParameterDeclaration;
  out Parameter: TMethodParameter);
const
  AccessModifierVAR = 'var';
  AccessModifierCONST = 'const';
  AccessModifierOUT = 'out';
var
  DataTypeInfo: TDataTypeInfo;
  DataTypeInfoAvailable, IsComplex, IsUntyped: Boolean;
  AnnotationInfo: TAnnotationInfo;
  InheritedPointerIndex, PointerIndex, DereferenceCount, I: Integer;
  AccessModifier, ParameterName, ProcessedDataTypeName, TempText: StdString;
begin
  InheritedPointerIndex := 0;
  DereferenceCount := 0;

  // Obtain data type info if such is available.
  DataTypeInfoAvailable := FDataTypes.GetInfo(Declaration.DataType, DataTypeInfo);

  if DataTypeInfoAvailable then
    InheritedPointerIndex := DataTypeInfo.PointerIndex;

  // Calculate current pointer index.
  PointerIndex := Declaration.PointerCount;
  Inc(PointerIndex, InheritedPointerIndex);

  // Obtain annotation info is such is available, or assume some options.
  if (Length(Declaration.Annotation) <= 0) or (not GetAnnotationInfo(Declaration.Annotation, AnnotationInfo)) then
  begin
    // Access is unknown for this data type.
    AnnotationInfo.Access := TAnnotationAccess.None;

    { No assumption can be made whether one or more parameters are expected or whether this parameter is optional.
      Therefore, it is assumed that parameter MAY be optional and MAY point to an array.}
    AnnotationInfo.Optional := Declaration.PointerCount > 0;
    AnnotationInfo.Multiple := Declaration.PointerCount > 0;
  end;

  // Determine if data type is complex enough so it can be passed by reference efficiently.
  if DataTypeInfoAvailable then
    IsComplex := Declaration.HasArraySizes or (DataTypeInfo.Group in [TDataTypeGroup.Structure,
      TDataTypeGroup.&Interface, TDataTypeGroup.Callback])
  else
    // Unknown data type should be considered complex anyway.
    IsComplex := True;

  IsUntyped := SameText(Declaration.DataType, CommonDataTypeVOID);

  { Interface is already a pointer, so it should be dereferenced. }
  if (DataTypeInfoAvailable and (DataTypeInfo.Group = TDataTypeGroup.&Interface)) or
    ((not DataTypeInfoAvailable) and IsDataTypeInterfaceGuess(Declaration.DataType)) then
    DereferencePointer(PointerIndex, DereferenceCount);

  // With access modifiers it is possible to pass complex structures by reference.
  AccessModifier := '';

  if (PointerIndex > 0) and (IsComplex or IsUntyped or (AnnotationInfo.Access in [TAnnotationAccess.Write,
    TAnnotationAccess.ReadWrite])) and (((not AnnotationInfo.Optional) and (not AnnotationInfo.Multiple)) or
    ((PointerIndex - InheritedPointerIndex) <= 0)) then
  begin
    case AnnotationInfo.Access of
      TAnnotationAccess.Read:
        AccessModifier := AccessModifierCONST;
      TAnnotationAccess.Write:
        AccessModifier := AccessModifierOUT;
     else
      AccessModifier := AccessModifierVAR;
    end;

    DereferencePointer(PointerIndex, DereferenceCount);
  end;

  // Translate data type.
  if DataTypeInfoAvailable then
    ProcessedDataTypeName := DataTypeInfo.Translated
  else
    ProcessedDataTypeName := Declaration.DataType;

  if ((not DataTypeInfoAvailable) or SameText(ProcessedDataTypeName, 'IntColor')) and
    (Length(ProcessedDataTypeName) > 0) then
  begin
    if (ProcessedDataTypeName[1] = '_') and (PointerIndex = 1) then
    begin
      Delete(ProcessedDataTypeName, 1, 1);

      if FObjectHandles.IndexOf(ProcessedDataTypeName) = -1 then
        FObjectHandles.Add(ProcessedDataTypeName);

      ProcessedDataTypeName := ProcessedDataTypeName + 'Rec';
    end;

    if (UpCase(ProcessedDataTypeName[1]) <> 'T') and (PointerIndex = 0) then
      ProcessedDataTypeName := 'T' + ProcessedDataTypeName;
  end;

  // Make sure parameter name is unique.
  ParameterName := Declaration.Name;
  UpdateParameterName(ParameterName, Declaration.DataType, ProcessedDataTypeName, PointerIndex, DereferenceCount,
    FParameterNames);

  // Add "P"s for each undereferenced pointer index (except for those already inherited).
  for I := 0 to (PointerIndex - InheritedPointerIndex) - 1 do
    ProcessedDataTypeName := AddPointerToDataType(ProcessedDataTypeName);

  // Untyped pointer that is singular and obligatory can be passed as such.
  if (PointerIndex > 0) and (not Declaration.HasArraySizes) and IsUntyped and (not AnnotationInfo.Optional) and
    (not AnnotationInfo.Multiple) then
    ProcessedDataTypeName := '';

  // Include array definition, if such is required.
  if Declaration.HasArraySizes then
  begin
    TempText := 'array';

    for I := 0 to Length(Declaration.ArraySizes) - 1 do
    begin
      if I = 0 then
        TempText := TempText + '[0..'
      else
        TempText := TempText + ', 0..';

      try
        TempText := TempText + IntToStr(StrToInt(Declaration.ArraySizes[I]) - 1);
      except
        TempText := TempText + Declaration.ArraySizes[I] + ' - 1';
      end;
    end;

    ProcessedDataTypeName := TempText + '] of ' + ProcessedDataTypeName;
  end;

  Parameter := TMethodParameter.Create(ParameterName, Declaration.DataType, ProcessedDataTypeName, AccessModifier);
end;

procedure THeaderParser.ConvertParameterDeclarationIntoStructParameter(const Declaration: TParameterDeclaration;
  const DataTypeOffset: Integer; out Parameter: TNormalParameter);
var
  DataTypeInfo: TDataTypeInfo;
  DataTypeInfoAvailable: Boolean;
  InheritedPointerIndex, PointerIndex, DereferenceCount, I, DataTypeSize: Integer;
  ParameterName, TranslatedDataTypeName: StdString;
begin
  InheritedPointerIndex := 0;
  DereferenceCount := 0;

  // Obtain data type info if such is available.
  DataTypeInfoAvailable := FDataTypes.GetInfo(Declaration.DataType, DataTypeInfo);

  if DataTypeInfoAvailable then
    InheritedPointerIndex := DataTypeInfo.PointerIndex;

  // Calculate current pointer index.
  PointerIndex := Declaration.PointerCount;
  Inc(PointerIndex, InheritedPointerIndex);

  { Interface is already a pointer, so it should be dereferenced. }
  if (DataTypeInfoAvailable and (DataTypeInfo.Group = TDataTypeGroup.&Interface)) or
    ((not DataTypeInfoAvailable) and IsDataTypeInterfaceGuess(Declaration.DataType)) then
    DereferencePointer(PointerIndex, DereferenceCount);

  // Translate data type.
  if DataTypeInfoAvailable then
  begin
    TranslatedDataTypeName := DataTypeInfo.Translated;
    DataTypeSize := DataTypeInfo.Size;
  end
  else
  begin
    TranslatedDataTypeName := Declaration.DataType;
    DataTypeSize := UnknownDataTypeSize;
  end;

  // Make sure parameter name is unique.
  ParameterName := Declaration.Name;
  UpdateParameterName(ParameterName, Declaration.DataType, TranslatedDataTypeName, PointerIndex, DereferenceCount,
    FParameterNames);

  // Create and configure resulting parameter.
  Parameter := TNormalParameter.Create(ParameterName, Declaration.DataType, TranslatedDataTypeName, DataTypeSize,
    DataTypeOffset, PointerIndex);

  for I := 0 to Length(Declaration.ArraySizes) - 1 do
    Parameter.AddArray(Declaration.ArraySizes[I]);
end;

procedure THeaderParser.ProcessDeclarationMethodsParametersToList;
var
  GenericParameterIndex, SavedPos: Integer;
  MoreParamsExpected, EndExpected: Boolean;
  Parameter: TMethodParameter;
  Declaration: TParameterDeclaration;
begin
  FMethodParameters.Clear;
  FParameterNames.Clear;

  MoreParamsExpected := False;
  EndExpected := False;

  GenericParameterIndex := 1;

  while not IsEndOfFile do
  begin
    SkipWhitespaceAndLineBreaks;

    if SkipComments(False) then
      Continue;

    // ')' end of parameters declaration.
    if AdvanceWithChar(')') then
    begin
      if MoreParamsExpected then
        RaiseException(EIdentifierExpected, SIdentifierExpected);

      SkipWhitespaceAndLineBreaks;
      Exit;
    end;

    if EndExpected then
      ExpectedException(')');

    if MethodDeclarationEndsWidthVoid then
    begin
      if MoreParamsExpected then
        RaiseException(EIdentifierExpected, SIdentifierExpected);

      if FMethodParameters.Count > 0 then
        RaiseException(EIdentifierExpected, SIdentifierExpected);

      Exit;
    end;

    SavedPos := FTextPos;

    if not ProcessMethodParameterDeclaration(@GenericParameterIndex, Declaration) then
      RaiseException(EDeclarationExpected, SDeclarationExpected, SavedPos);

    ConvertParameterDeclarationIntoMethodParameter(Declaration, Parameter);

    if FParameterNames.IndexOf(Parameter.Name) = -1 then
      FParameterNames.Add(Parameter.Name);

    if FParameterNames.IndexOf(Parameter.DataType) = -1 then
      FParameterNames.Add(Parameter.DataType);

    FMethodParameters.Add(Parameter);

    if AdvanceWithChar('=') then
    begin
      SkipWhitespaceAndLineBreaks;
      ParseExpression(False);
    end;

    // More parameters exist?
    if AdvanceWithChar(',') then
      MoreParamsExpected := True
    else
    begin
      MoreParamsExpected := False;
      EndExpected := True;
    end;
  end;
end;

function THeaderParser.ProcessDeclarationMethodsParameters(out ANewLineWidth: Integer;
  const ALineWidth: Integer): string;
var
  I, LLineWidth: Integer;
  ParamDeclContinue: Boolean;
  LText: string;
begin
  ProcessDeclarationMethodsParametersToList;

  Result := '';
  LLineWidth := ALineWidth;

  if FMethodParameters.Count > 0 then
  begin
    Result := Result + '(';
    Inc(LLineWidth);
    Inc(FSideSpace, DefaultTabWidth);
    try
      for I := 0 to FMethodParameters.Count - 1 do
      begin
        LText := '';

        if Length(FMethodParameters[I].AccessModifier) > 0 then
          LText := LText + FMethodParameters[I].AccessModifier + ' ';

        if (I < FMethodParameters.Count - 1) and
          (FMethodParameters[I].ProcessedDataType = FMethodParameters[I + 1].ProcessedDataType) and
          (FMethodParameters[I].AccessModifier = FMethodParameters[I + 1].AccessModifier) then
        begin
          LText := LText + FMethodParameters[I].Name;
          ParamDeclContinue := True;
        end
        else
        begin
          if Length(FMethodParameters[I].ProcessedDataType) > 0 then
            LText := LText + FMethodParameters[I].Name + ': ' + FMethodParameters[I].ProcessedDataType
          else
            LText := LText + FMethodParameters[I].Name;
          ParamDeclContinue := False;
        end;

        if I < FMethodParameters.Count - 1 then
        begin
          if ParamDeclContinue then
            LText := LText + ','
          else
            LText := LText + ';';
        end
        else
          LText := LText + ')';

        if I < FMethodParameters.Count - 1 then
          LText := LText + ' ';

        if LLineWidth + Length(LText) > FLineLimit then
        begin
          Result := Result + SLineBreak;
          LLineWidth := 0;

          Result := Result + SideSpaceText;
          Inc(LLineWidth, FSideSpace);

{          if I < FMethodParameters.Count - 1 then
          begin
          end;}
        end;

        Result := Result + LText;
        Inc(LLineWidth, Length(LText));
      end;
    finally
      Dec(FSideSpace, DefaultTabWidth);
    end;
  end;
  ANewLineWidth := LLineWidth;
end;

procedure THeaderParser.ProcessStatementMIDLInterfaceMethods;

  function FunctionEndText(const CallingType: TCallingType; const VoidMethod: Boolean;
    const ResultType: StdString): StdString;
  begin
    if not VoidMethod then
      Result := ': ' + ResultType + ';'
    else
      Result := ';';

    if CallingType = TCallingType.WinStd then
      Result := Result + ' stdcall;';
  end;

const
  TypeSTDMETHODCALLTYPE = 'STDMETHODCALLTYPE';
var
  Identifier, ResultType, MethodName, TextToAdd: StdString;
  CallingType: TCallingType;
  VoidMethod, ParamDeclContinue: Boolean;
  DataTypeInfo: TDataTypeInfo;
  I, CurLineWidth: Integer;
begin
  while not IsEndOfFile do
  begin
    SkipWhitespaceAndLineBreaks;

    if SkipComments(False) then
      Continue;

    if AdvanceWithChar('}') then
    begin
      SkipWhitespace;
      ProcessExpectedChar(';');
      Exit;
    end;

    Identifier := ProcessIdentifier;

    // "public:"
    if SameText(Identifier, 'public') then
    begin
      // Skip ':'
      SkipWhitespace;
      ProcessExpectedChar(':');
      Continue;
    end;

    // "virtual"
    if SameText(Identifier, 'virtual') then
    begin
      SkipWhitespaceAndLineBreaks;

      if SkipComments(False) then
        SkipWhitespaceAndLineBreaks;

      // Result Type
      ResultType := ProcessIdentifier;

      // Resolve multiple-word data types.
      ResultType := ProcessMultiWordDataType(ResultType);
      SkipWhitespaceAndLineBreaks;

      // Calling Convention and Method Name
      CallingType := TCallingType.Normal;
      MethodName := ProcessIdentifier;

      // "STDMETHODCALLTYPE" modifier
      if SameText(MethodName, TypeSTDMETHODCALLTYPE) then
      begin
        CallingType := TCallingType.WinStd;

        SkipWhitespaceAndLineBreaks;
        MethodName := ProcessIdentifier;
      end;

      ProcessExpectedChar('(');

      ProcessDeclarationMethodsParametersToList;

      VoidMethod := SameText(ResultType, 'void');

      if FDataTypes.GetInfo(ResultType, DataTypeInfo) then
        ResultType := TranslateDataType(ResultType, DataTypeInfo.Translated, 0, 0)
      else
        ResultType := TranslateDataType(ResultType, ResultType, 0, 0);

      if IsReservedWord(MethodName) then
        MethodName := '&' + MethodName;

      if not VoidMethod then
      begin
        AddText(SideSpaceText + 'function ' + MethodName);
        CurLineWidth := FSideSpace + 9 + Length(MethodName);
      end
      else
      begin
        AddText(SideSpaceText + 'procedure ' + MethodName);
        CurLineWidth := FSideSpace + 10 + Length(MethodName);
      end;

      if FMethodParameters.Count > 0 then
      begin
        AddText('(');
        Inc(CurLineWidth);
        ParamDeclContinue := False;

        for I := 0 to FMethodParameters.Count - 1 do
        begin
          if (Length(FMethodParameters[I].AccessModifier) > 0) and (not ParamDeclContinue) then
            TextToAdd := FMethodParameters[I].AccessModifier + ' '
          else
            TextToAdd := '';

          if (I < FMethodParameters.Count - 1) and
            (FMethodParameters[I].ProcessedDataType = FMethodParameters[I + 1].ProcessedDataType) and
            (FMethodParameters[I].AccessModifier = FMethodParameters[I + 1].AccessModifier) then
          begin
            TextToAdd := TextToAdd + FMethodParameters[I].Name;
            ParamDeclContinue := True;
          end
          else
          begin
            if Length(FMethodParameters[I].ProcessedDataType) > 0 then
              TextToAdd := TextToAdd + FMethodParameters[I].Name + ': ' + FMethodParameters[I].ProcessedDataType
            else
              TextToAdd := TextToAdd + FMethodParameters[I].Name;

            ParamDeclContinue := False;
          end;

          if I < FMethodParameters.Count - 1 then
          begin
            if ParamDeclContinue then
              TextToAdd := TextToAdd + ','
            else
              TextToAdd := TextToAdd + ';';
          end
          else
            TextToAdd := TextToAdd + ')' + FunctionEndText(CallingType, VoidMethod, ResultType);

          if CurLineWidth + Length(TextToAdd) > 118 then
          begin
            AddLineBreak;
            AddText(SideSpaceText(DefaultTabWidth * 3));
            CurLineWidth := DefaultTabWidth * 3;
          end;

          if I < FMethodParameters.Count - 1 then
            TextToAdd := TextToAdd + ' ';

          AddText(TextToAdd);
          Inc(CurLineWidth, Length(TextToAdd));
        end;
      end
      else
        AddText(FunctionEndText(CallingType, VoidMethod, ResultType));

      // Check if there is unusual "= [value]" statement after declaration.
      if AdvanceWithChar('=') then
      begin
        SkipWhitespaceAndLineBreaks;
        ProcessNumericalValue;
      end;

      AddLineBreak;

      ProcessExpectedChar(';');

      Continue;
    end;

    RaiseException(ECannotInterpret, SCannotInterpret);
  end;
end;

procedure THeaderParser.ProcessStatementMIDLInterface;
var
  GUIDValue, IntfcName, VisibilityName, InheritedName: StdString;
begin
  // Skip '(' and '"' characters.
  SkipWhitespace;
  ProcessExpectedChar('(');
  SkipWhitespace;
  ProcessExpectedChar('"');

  // GUID
  GUIDValue := ProcessGUIDValue;

  // Skip '"' and ')' characters
  ProcessExpectedChar('"');
  SkipWhitespace;
  ProcessExpectedChar(')');
  SkipWhitespaceAndLineBreaks;

  // Interface Name
  IntfcName := ProcessIdentifier;
  SkipWhitespaceAndLineBreaks;

  // Skip ':'
  ProcessExpectedChar(':');
  SkipWhitespaceAndLineBreaks;

  // Interface Visibility
  VisibilityName := ProcessIdentifier;
  SkipWhitespaceAndLineBreaks;

  // Inherited Class
  InheritedName := ProcessIdentifier;
  SkipWhitespaceAndLineBreaks;

  // Declare first part of interface
  FDataTypes.Define(IntfcName, '', IntfcName, DefaultPointerDataTypeSize, 0, TDataTypeGroup.&Interface,
    TDataTypeRelation.Declared);

  StartCodeSection(TCodeSection.Constants_SID);
  StartSection(TSection.Constants, TSubSection.SIDs);

  AddLine(SideSpaceText + 'SID_' + IntfcName + ' = ''{' + GUIDValue + '}'';');

  StartCodeSection(TCodeSection.Constants_IID);
  StartSection(TSection.Constants, TSubSection.IIDs);

  AddLine(SideSpaceText + 'IID_' + IntfcName + ': TGuid = SID_' +  IntfcName + ';');

  StartCodeSection(TCodeSection.Types_Interfaces, IntfcName);
  StartSection(TSection.Types, TSubSection.Interfaces);

  AddLine(SideSpaceText + 'P' + IntfcName + ' = ^' + IntfcName + ';');
  AddLine(SideSpaceText + IntfcName + ' = interface(' + InheritedName + ')');

  Inc(FSideSpace, DefaultTabWidth);
  try
//    AddLine(SideSpaceText + '[''{' + GUIDValue + '}'']');
    AddLine(SideSpaceText + '[SID_' + IntfcName + ']');

    // Skip '{'
    ProcessExpectedChar('{');

    ProcessStatementMIDLInterfaceMethods;
  finally
    Dec(FSideSpace, DefaultTabWidth);
  end;

  AddLine(SideSpaceText + 'end;');
end;

procedure THeaderParser.ProcessStatementSTDInterfaceMethods;
type
  TSTDMethodType = (None, HasResultType, HasHResult);

  function FunctionEndText(const VoidMethod: Boolean; const ResultType: StdString): StdString;
  begin
    if not VoidMethod then
      Result := ': ' + ResultType + ';'
    else
      Result := ';';

    Result := Result + ' stdcall;';
  end;

const
  StatementSTDMETHOD_ = 'STDMETHOD_';
  StatementSTDMETHOD = 'STDMETHOD';
  StatementCOMDECLSPEC_NOTHROW = 'COM_DECLSPEC_NOTHROW';
  StatementTHIS1 = 'THIS_';
  StatementTHIS2 = 'THIS';
var
  Identifier, ResultType, MethodName, TextToAdd: StdString;
  VoidMethod, ParamDeclContinue, UnsupportedResultType: Boolean;
  DataTypeInfo: TDataTypeInfo;
  I, CurLineWidth, ResultPointerIndex: Integer;
  MethodType: TSTDMethodType;
begin
  while not IsEndOfFile do
  begin
    SkipWhitespaceAndLineBreaks;

    if SkipComments(False) then
      Continue;

    if AdvanceWithChar('}') then
    begin
      SkipWhitespace;
      ProcessExpectedChar(';');
      Exit;
    end;

    Identifier := ProcessIdentifier;

    MethodType := TSTDMethodType.None;

    if SameText(Identifier, StatementSTDMETHOD_) then
      MethodType := TSTDMethodType.HasResultType
    else if SameText(Identifier, StatementSTDMETHOD) then
      MethodType := TSTDMethodType.HasHResult;

    if (MethodType = TSTDMethodType.None) and SameText(Identifier, StatementCOMDECLSPEC_NOTHROW) then
    begin
      ProcessSkipOpenCloseBrackets(False);
      ProcessSkipOpenCloseBrackets(False, '{', '}');
      Continue;
    end;

    // "STDMETHOD_"
    if MethodType <> TSTDMethodType.None then
    begin
      SkipWhitespaceAndLineBreaks;
      ProcessExpectedChar('(');

      ResultPointerIndex := 0;

      if MethodType = TSTDMethodType.HasResultType then
      begin
        // Result Type
        ResultType := ProcessIdentifier;

        // Resolve multiple-word data types.
        ResultType := ProcessMultiWordDataType(ResultType);
        SkipWhitespaceAndLineBreaks;

        // '*' ponter refences
        ResultPointerIndex := ProcessPointerReferences;

        ProcessExpectedChar(',');
        SkipWhitespaceAndLineBreaks;
      end
      else
        ResultType := 'HRESULT';

      // Calling Convention and Method Name
      MethodName := ProcessIdentifier;
      SkipWhitespaceAndLineBreaks;

      ProcessExpectedChar(')');
      SkipWhitespaceAndLineBreaks;
      ProcessExpectedChar('(');
      SkipWhitespaceAndLineBreaks;

      if AdvanceWithText(StatementTHIS1) or AdvanceWithText(StatementTHIS2) then
        SkipWhitespaceAndLineBreaks;

      ProcessDeclarationMethodsParametersToList;

      VoidMethod := SameText(ResultType, 'void');
      UnsupportedResultType := IsDataTypeComplex(ResultType);

      if FDataTypes.GetInfo(ResultType, DataTypeInfo) then
      begin
        if (DataTypeInfo.Group = TDataTypeGroup.&Interface) and (ResultPointerIndex > 0) then
          Dec(ResultPointerIndex);

        ResultType := TranslateDataType(ResultType, DataTypeInfo.Translated, ResultPointerIndex, 0);

        if UnsupportedResultType and (DataTypeInfo.Group = TDataTypeGroup.&Interface) then
          UnsupportedResultType := False;
      end
      else
        ResultType := TranslateDataType(ResultType, ResultType, 0, 0);

      for I := 0 to ResultPointerIndex - 1 do
        ResultType := AddPointerToDataType(ResultType);

      if UnsupportedResultType then
      begin
        VoidMethod := True;
        FMethodParameters.Add(TMethodParameter.Create('_Result', '', ResultType, 'out'));

        ResultType := '';
      end;

      if IsReservedWord(MethodName) then
        MethodName := '&' + MethodName;

      if not VoidMethod then
      begin
        AddText(SideSpaceText + 'function ' + MethodName);
        CurLineWidth := FSideSpace + 9 + Length(MethodName);
      end
      else
      begin
        AddText(SideSpaceText + 'procedure ' + MethodName);
        CurLineWidth := FSideSpace + 10 + Length(MethodName);
      end;

      if FMethodParameters.Count > 0 then
      begin
        AddText('(');
        Inc(CurLineWidth);

        ParamDeclContinue := False;

        for I := 0 to FMethodParameters.Count - 1 do
        begin
          if (Length(FMethodParameters[I].AccessModifier) > 0) and (not ParamDeclContinue) then
            TextToAdd := FMethodParameters[I].AccessModifier + ' '
          else
            TextToAdd := '';

          if (I < FMethodParameters.Count - 1) and
            (FMethodParameters[I].ProcessedDataType = FMethodParameters[I + 1].ProcessedDataType) and
            (FMethodParameters[I].AccessModifier = FMethodParameters[I + 1].AccessModifier) then
          begin
            TextToAdd := TextToAdd + FMethodParameters[I].Name;
            ParamDeclContinue := True;
          end
          else
          begin
            TextToAdd := TextToAdd + FMethodParameters[I].Name + ': ' + FMethodParameters[I].ProcessedDataType;
            ParamDeclContinue := False;
          end;

          if I < FMethodParameters.Count - 1 then
          begin
            if ParamDeclContinue then
              TextToAdd := TextToAdd + ','
            else
              TextToAdd := TextToAdd + ';';
          end
          else
            TextToAdd := TextToAdd + ')' + FunctionEndText(VoidMethod, ResultType);

          if CurLineWidth + Length(TextToAdd) > 118 then
          begin
            AddLineBreak;
            AddText(SideSpaceText(DefaultTabWidth * 3));
            CurLineWidth := DefaultTabWidth * 3;
          end;

          if I < FMethodParameters.Count - 1 then
            TextToAdd := TextToAdd + ' ';

          AddText(TextToAdd);
          Inc(CurLineWidth, Length(TextToAdd));
        end;
      end
      else
        AddText(FunctionEndText(VoidMethod, ResultType));

      // Check if there is unusual "= [value]" statement after declaration.
      if AdvanceWithChar('=') then
      begin
        SkipWhitespaceAndLineBreaks;
        ProcessNumericalValue;
      end;

      AddLineBreak;

      SkipAfterNextCharInstance(';');

      Continue;
    end;

    RaiseException(ECannotInterpret, SCannotInterpret);
  end;
end;

procedure THeaderParser.ProcessStatementSTDInterface;
const
  Statement_DX_DECLARE_INTERFACE = 'DX_DECLARE_INTERFACE';
  Statement_DWRITE_DECLARE_INTERFACE = 'DWRITE_DECLARE_INTERFACE';
  Statement_DECLSPEC_UUID = 'DECLSPEC_UUID';
  Statement_DECLSPEC_NOVTABLE = 'DECLSPEC_NOVTABLE';
var
  Identifier, GUIDValue, IntfcName, VisibilityName, InheritedName: StdString;
begin
  // DX_DECLARE_INTERFACE, DWRITE_DECLARE_INTERFACE
  if (not StartsWithText(Statement_DX_DECLARE_INTERFACE)) and
    (not StartsWithText(Statement_DWRITE_DECLARE_INTERFACE)) and (not StartsWithText(Statement_DECLSPEC_UUID)) then
  begin
    ProcessStatementTypedefINTERFACE(True);
    Exit;
  end;

  // Skip aforementioned declaration.
  Identifier := ProcessIdentifier;

  // Skip '(' and '"' characters.
  SkipWhitespace;
  ProcessExpectedChar('(');
  SkipWhitespace;
  ProcessExpectedChar('"');

  // GUID
  GUIDValue := ProcessGUIDValue;

  // Skip '"' and ')' characters
  ProcessExpectedChar('"');
  SkipWhitespace;
  ProcessExpectedChar(')');
  SkipWhitespaceAndLineBreaks;

  // Interface Name
  IntfcName := ProcessIdentifier;
  SkipWhitespaceAndLineBreaks;

  // Skip 'DECLSPEC_NOVTABLE'
  if SameText(IntfcName, Statement_DECLSPEC_NOVTABLE) then
  begin
    IntfcName := ProcessIdentifier;
    SkipWhitespaceAndLineBreaks;
  end;

  // ';' ends declaration, this is a forward;
  if AdvanceWithChar(';') then
  begin
    AddDebugText('Skipping ' + IntfcName + ' = interface statement (empty DECLSPEC_UUID).');
    Exit;
  end;

  // Skip ':'
  ProcessExpectedChar(':');
  SkipWhitespaceAndLineBreaks;

  // Interface Visibility
  VisibilityName := ProcessIdentifier;
  SkipWhitespaceAndLineBreaks;

  // Inherited Class
  InheritedName := ProcessIdentifier;
  SkipWhitespaceAndLineBreaks;

  // Declare first part of interface
  FDataTypes.Define(IntfcName, '', IntfcName, DefaultPointerDataTypeSize, 0, TDataTypeGroup.&Interface,
    TDataTypeRelation.Declared);

  if Length(GUIDValue) > 0 then
  begin
    StartCodeSection(TCodeSection.Constants_SID);
    StartSection(TSection.Constants, TSubSection.SIDs);

    AddLine(SideSpaceText + 'SID_' + IntfcName + ' = ''{' + GUIDValue + '}'';');

    StartCodeSection(TCodeSection.Constants_IID);
    StartSection(TSection.Types, TSubSection.IIDs);

    AddLine(SideSpaceText + 'IID_' + IntfcName + ': TGuid = SID_' +  IntfcName + ';');
  end;

  StartCodeSection(TCodeSection.Types_Interfaces, IntfcName);
  StartSection(TSection.Types, TSubSection.Interfaces);

  AddLine(SideSpaceText + 'P' + IntfcName + ' = ^' + IntfcName + ';');

  if Length(InheritedName) > 0 then
    AddLine(SideSpaceText + IntfcName + ' = interface(' + InheritedName + ')')
  else
    AddLine(SideSpaceText + IntfcName + ' = interface');

  Inc(FSideSpace, DefaultTabWidth);
  try
    AddLine(SideSpaceText + '[SID_' + IntfcName + ']');

    // Skip '{'
    ProcessExpectedChar('{');

    ProcessStatementSTDInterfaceMethods;
  finally
    Dec(FSideSpace, DefaultTabWidth);
  end;

  AddLine(SideSpaceText + 'end;');
end;

procedure THeaderParser.ProcessStatementSTDLegacyInterface;
var
  IntfcName, InheritedName: StdString;
begin
  SkipWhitespace;
  ProcessExpectedChar('(');
  SkipWhitespaceAndLineBreaks;

  // Interface Name
  IntfcName := ProcessIdentifier;
  SkipWhitespaceAndLineBreaks;

  if AdvanceWithChar(',') then
  begin
    SkipWhitespaceAndLineBreaks;

    // Inherited Class
    InheritedName := ProcessIdentifier;
    SkipWhitespaceAndLineBreaks;
  end
  else
    InheritedName := '';

  ProcessExpectedChar(')');
  SkipWhitespaceAndLineBreaks;

  // Declare first part of interface
  FDataTypes.Define(IntfcName, '', IntfcName, DefaultPointerDataTypeSize, 0, TDataTypeGroup.&Interface,
    TDataTypeRelation.Declared);

  StartCodeSection(TCodeSection.Types_Interfaces, IntfcName);
  StartSection(TSection.Types, TSubSection.Interfaces);

  AddLine(SideSpaceText + 'P' + IntfcName + ' = ^' + IntfcName + ';');

  if Length(InheritedName) > 0 then
    AddLine(SideSpaceText + IntfcName + ' = interface(' + InheritedName + ')')
  else
    AddLine(SideSpaceText + IntfcName + ' = interface');

  Inc(FSideSpace, DefaultTabWidth);
  try
    AddLine(SideSpaceText + '[SID_' + IntfcName + ']');

    // Skip '{'
    ProcessExpectedChar('{');

    ProcessStatementSTDInterfaceMethods;
  finally
    Dec(FSideSpace, DefaultTabWidth);
  end;

  AddLine(SideSpaceText + 'end;');
end;

procedure THeaderParser.ProcessStatementDefineGUID;
var
  ConstName, Value, GUIDText, ConstantSID: StdString;
  I: Integer;
begin
  // Skip '('
  ProcessExpectedChar('(');
  SkipWhitespace;

  ConstName := ProcessIdentifier;
  SkipWhitespace;

  ProcessExpectedChar(',');
  SkipWhitespaceAndLineBreaks;

  GUIDText := '''{';

  Value := ProcessGuidIntegerValue(8);
  SkipWhitespaceAndLineBreaks;

  ProcessExpectedChar(',');
  SkipWhitespaceAndLineBreaks;

  GUIDText := GUIDText + Value + '-';

  Value := ProcessGuidIntegerValue(4);
  SkipWhitespaceAndLineBreaks;

  ProcessExpectedChar(',');
  SkipWhitespaceAndLineBreaks;

  GUIDText := GUIDText + Value + '-';

  Value := ProcessGuidIntegerValue(4);
  SkipWhitespaceAndLineBreaks;

  ProcessExpectedChar(',');
  SkipWhitespaceAndLineBreaks;

  GUIDText := GUIDText + Value + '-';

  for I := 0 to 7 do
  begin
    Value := ProcessGuidIntegerValue(2);
    SkipWhitespaceAndLineBreaks;

    GUIDText := GUIDText + Value;

    if I < 7 then
    begin
      if I = 1 then
        GUIDText := GUIDText + '-';

      ProcessExpectedChar(',');
      SkipWhitespaceAndLineBreaks;
    end;
  end;

  GUIDText := GUIDText + '}''';

  ProcessExpectedChar(')');
  ProcessExpectedChar(';');

  if CodeStartsWithText(ConstName, 1, 'IID_') then
  begin
    ConstantSID := ConstName;
    ConstantSID[1] := 'S';

    StartCodeSection(TCodeSection.Constants_SID);
    StartSection(TSection.Constants, TSubSection.SIDs);

    AddLine(SideSpaceText + ConstantSID + ' = ' + GUIDText + ';');

    StartCodeSection(TCodeSection.Constants_IID);
    StartSection(TSection.Constants, TSubSection.IIDs);

    AddLine(SideSpaceText + ConstName + ': TGuid = ' + ConstantSID + ';');
  end
  else
  begin
    StartCodeSection(TCodeSection.Constants_GUID);
    StartSection(TSection.Constants, TSubSection.GUIDs);

    AddLine(SideSpaceText + ConstName + ': TGuid = ' + GUIDText + ';');
  end;
end;

procedure THeaderParser.ProcessGlobalFunction;
const
  ConventionsWINAPI = 'WINAPI';
  ConventionsEMBAPI = 'EMBAPI';
  ConventionsAFTERWARPAPI = 'AFTERWARP_API';
  DataTypeVOID = 'void';
var
  ResultType, MethodName: StdString;
  CallingType: TCallingType;
  VoidMethod: Boolean;
  DataTypeInfo: TDataTypeInfo;
  I, LPointerIndex, LLineWidth: Integer;
  LText: string;
begin
  CallingType := TCallingType.Normal;

  // Result Type
  ResultType := ProcessIdentifier;

  // "AFTERWARP_API" conventions
  if SameText(ResultType, ConventionsAFTERWARPAPI) then
  begin
    CallingType := TCallingType.Normal;
    SkipWhitespaceAndLineBreaks;
    ResultType := ProcessIdentifier;
  end;

  // Resolve multiple-word data types.
  ResultType := ProcessMultiWordDataType(ResultType);
  SkipWhitespaceAndLineBreaks;
  // '*' pointer references before CONST
  LPointerIndex := ProcessPointerReferences;

  // Calling Convention and Method Name
  MethodName := ProcessIdentifier;

  // "WINAPI" conventions
  if SameText(MethodName, ConventionsWINAPI) or SameText(MethodName, ConventionsEMBAPI) then
  begin
    CallingType := TCallingType.WinStd;

    SkipWhitespaceAndLineBreaks;
    MethodName := ProcessIdentifier;
  end;

  SkipWhitespaceAndLineBreaks;
  ProcessExpectedChar('(');

  VoidMethod := SameText(ResultType, DataTypeVOID);

  if FDataTypes.GetInfo(ResultType, DataTypeInfo) then
    ResultType := TranslateDataType(ResultType, DataTypeInfo.Translated, LPointerIndex, 0)
  else
    ResultType := TranslateDataType(ResultType, ResultType, LPointerIndex, 0);

  for I := 0 to LPointerIndex - 1 do
    ResultType := AddPointerToDataType(ResultType);

  StartCodeSection(TCodeSection.Functions, MethodName);
  StartSection(TSection.Functions);

  if not VoidMethod then
    AddText('function ')
  else
    AddText('procedure ');

  AddText(MethodName);

  AddText(ProcessDeclarationMethodsParameters(LLineWidth, FLineWidth));
  FLineWidth := LLineWidth;

  if not VoidMethod then
  begin
    LText := ': ' + ResultType + ';';

    if FLineWidth > FLineLimit - Length(LText) then
    begin
      AddLineBreak;
      AddText(SideSpaceText);
      Inc(FLineWidth, FSideSpace);
      LText := SideSpaceText(DefaultTabWidth) + LText;
    end;
    AddText(LText);
  end
  else
    AddText(';');

  // cdecl
  LText := ' cdecl;';

//  if CallingType = TCallingType.WinStd then
//    LText := ' stdcall;';

  if FLineWidth > FLineLimit - Length(LText) then
  begin
    AddLineBreak;
    AddText(SideSpaceText);
    Inc(FLineWidth, FSideSpace);

    Delete(LText, 1, 1);
    LText := SideSpaceText(DefaultTabWidth) + LText;
  end;

  AddText(LText);

  // external symbol
  LText := ' external AfterwarpLib;';

  if FLineWidth > FLineLimit - Length(LText) then
  begin
    AddLineBreak;
    AddText(SideSpaceText);
    Inc(FLineWidth, FSideSpace);

    Delete(LText, 1, 1);
    LText := SideSpaceText(DefaultTabWidth) + LText;
  end;

  AddText(LText);
  AddLineBreak;


{  AddText(SideSpaceText + 'T' + MethodName + ' = ');

  if not VoidMethod then
    AddText('function')
  else
    AddText('procedure');

  ParamText := ProcessDeclarationMethodsParameters;

  AddText(ParamText);

  if not VoidMethod then
    AddText(': ' + ResultType + ';')
  else
    AddText(';');

  if CallingType = TCallingType.WinStd then
    AddText(' stdcall;');}


{
  StartCodeSection(TCodeSection.Functions, MethodName);
  StartSection(TSection.Variables);

  AddLine(SideSpaceText + MethodName + ': T' + MethodName + ' = nil;');}

  ProcessExpectedChar(';');
end;

procedure THeaderParser.DeclareObjectHandles;
var
  LHandle, LText: string;
  I: Integer;
begin
  LText := '';

  if FObjectHandles.Count > 0 then
    LText := 'type' + SLineBreak;

  for I := 0 to FObjectHandles.Count - 1 do
  begin
    LHandle := FObjectHandles[I];

    LText := LText + SideSpaceText(DefaultTabWidth) + 'P' + LHandle + 'Rec = ^T' + LHandle + 'Rec;' + SLineBreak;
    LText := LText + SideSpaceText(DefaultTabWidth) + 'T' + LHandle + 'Rec = record' + SLineBreak;
    LText := LText + SideSpaceText(DefaultTabWidth) + 'end;' + SLineBreak;

    LText := LText + SLineBreak;
  end;

  FFinalCode := LText + FFinalCode;
end;

procedure THeaderParser.Process;
const
  StatementTYPEDEF = 'typedef';
  StatementSTRUCT = 'struct';
  Statement_MIDL_INTERFACE = 'MIDL_INTERFACE';
  Statement_DEFINE_GUID = 'DEFINE_GUID';
  Statement_STDINTERFACE = 'interface';
  Statement_DECLARE_INTERFACE1 = 'DECLARE_INTERFACE';
  Statement_DECLARE_INTERFACE2 = 'DECLARE_INTERFACE_';
var
  SavedPos: Integer;
  InvalidLine: Boolean;
//  CodeSection: TCodeSection;
begin
  FTextPos := 1;

{  FSection := TSection.None;
  FSubSection := TSubSection.None;
  FCodeSection := TCodeSection.None;

  for CodeSection := Low(TCodeSection) to High(TCodeSection) do
  begin
    FCodeSymbol[CodeSection] := '';
    FCodeBelongs[CodeSection] := TSubSection.None;
  end;}

  while not IsEndOfFile do
  begin
    SkipWhitespaceAndLineBreaks;
    if IsEndOfFile then
      Break;

    if SkipComments((TParserOption.LeaveComments in FOptions) and (FDefineLevelSkip = -1)) then
      Continue;

    if SkipUnsupportedDirectives then
      Continue;

    if ProcessConditionals then
      Continue;

    // typedef
    if AdvanceWithText(StatementTYPEDEF) then
    begin
      ProcessStatementTYPEDEF;
      Continue;
    end;

    // struct
    if AdvanceWithText(StatementSTRUCT) then
    begin
      SkipWhitespaceAndLineBreaks;
      ProcessStatementTypedefSTRUCT;
      Continue;
    end;

    // MIDL_INTERFACE
    if AdvanceWithText(Statement_MIDL_INTERFACE) then
    begin
      ProcessStatementMIDLInterface;
      Continue;
    end;

    // interface
    if AdvanceWithText(Statement_STDINTERFACE) then
    begin
      SkipWhitespaceAndLineBreaks;

      ProcessStatementSTDInterface;
      Continue;
    end;

    // DECLARE_INTERFACE
    if AdvanceWithText(Statement_DECLARE_INTERFACE2) or AdvanceWithText(Statement_DECLARE_INTERFACE1) then
    begin
      ProcessStatementSTDLegacyInterface;
      Continue;
    end;

    // DEFINE_GUID
    if AdvanceWithText(Statement_DEFINE_GUID) then
    begin
      SkipWhitespace;
      ProcessStatementDefineGUID;
      Continue;
    end;

    // Global Function
    SavedPos := FTextPos;
    InvalidLine := False;

    try
      ProcessGlobalFunction;
    except
      InvalidLine := True;
    end;

    if InvalidLine then
    begin
      FTextPos := SavedPos;

      AddDebugText('Cannot interpret this line.');

      if TParserOption.LeaveUnsupportedSymbols in FOptions then
      begin
        StartCodeSection(TCodeSection.None);
        StartSection(TSection.UnsupportedSymbols);

        AddLine('// ' + RetrieveTextUntilEndOfLine);
      end
      else
        SkipToNextLine;
    end;
  end;

  DumpDataTypeAliases;
end;

procedure THeaderParser.CombineCode;

  procedure AddToText(var Text: StdString; const SubText: StdString);
  begin
    if Length(SubText) > 0 then
      if Length(Text) > 0 then
        Text := Text + SLineBreak + SubText
      else
        Text := SubText;
  end;

var
  Text: StdString;
  CodeSection: TCodeSection;
begin
  // Unidentified
  FFinalCode := FCode[TCodeSection.None];

  // Constants
  Text := '';

  AddToText(Text, FCode[TCodeSection.Constants_Defines]);
  AddToText(Text, FCode[TCodeSection.Constants_Enums]);
  AddToText(Text, FCode[TCodeSection.Constants_GUID]);
  AddToText(Text, FCode[TCodeSection.Constants_SID]);
  AddToText(Text, FCode[TCodeSection.Constants_IID]);

  if Length(Text) > 0 then
    AddToText(FFinalCode, 'const' + SLineBreak + Text);

  // Types
  Text := '';

  AddToText(Text, FCode[TCodeSection.Types_Forwards]);
  AddToText(Text, FCode[TCodeSection.Types_Redirects]);
  AddToText(Text, FCode[TCodeSection.Type_Enums]);
  AddToText(Text, FCode[TCodeSection.Types_Records]);
  AddToText(Text, FCode[TCodeSection.Types_Callbacks]);
  AddToText(Text, FCode[TCodeSection.Types_Interfaces]);

  if Length(Text) > 0 then
    AddToText(FFinalCode, 'type' + SLineBreak + Text);

  // Variables
  if Length(FCode[TCodeSection.Variables]) > 0 then
    AddToText(FFinalCode, 'var' + SLineBreak + FCode[TCodeSection.Variables]);

  // Functions
  if Length(FCode[TCodeSection.Functions]) > 0 then
    AddToText(FFinalCode, FCode[TCodeSection.Functions]);

  for CodeSection := Low(TCodeSection) to High(TCodeSection) do
    FCode[CodeSection] := '';
end;

procedure THeaderParser.DumpDataTypeAliases;
var
  I, J, FinalPointerIndex: Integer;
  Alias: TDataTypeAlias;
  DataType, FinalDataType: StdString;
begin
  for J := 0 to FDataTypeAliases.Count - 1 do
  begin
    Alias := FDataTypeAliases[J];

    FinalPointerIndex := 0;
    FinalDataType := Alias.Name;

    FDataTypeAliases.Resolve(FinalDataType, FinalPointerIndex);

    DataType := Alias.DataType;

    if SameText(DataType, FinalDataType) then
      FinalDataType := '';

    for I := 0 to Alias.PointerIndex - 1 do
      DataType := '^' + DataType;

    if Length(FinalDataType) > 0 then
    begin
      for I := 0 to FinalPointerIndex - 1 do
        FinalDataType := '^' + FinalDataType;

      AddDebugText('alias ' + Alias.Name + ' = ' + DataType + ' = ' + FinalDataType);
    end
    else
      AddDebugText('alias ' + Alias.Name + ' = ' + DataType);
  end;
end;

end.
