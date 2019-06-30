unit HPL.Types;
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

type
  StdString = UnicodeString;
  StdChar = Char;

  TSetOfChar = set of StdChar;
  TNumericalValueType = (None, Integer, Float, IEEEFloat, Hex);

  TCustomTextParser = class
  protected const
    SLineBreak = #13#10;
  protected
    FSourceCode: StdString;
    FTextPos: Integer;

    function IsEndOfFile: Boolean;
    procedure SkipWhitespace;
    procedure SkipWhitespaceAndLineBreaks;
    procedure SkipToNextLine;
    procedure Advance(const Delta: Integer = 1);

    function StartsWithText(const Text: StdString): Boolean; inline;
    function StartsWithChar(const Ch: StdChar): Boolean; inline;

    function AdvanceWithText(const Text: StdString): Boolean;
    function AdvanceWithChar(const Ch: StdChar): Boolean;

    function SkipAfterNextTextInstance(const Text: StdString): Boolean;
    function SkipAfterNextCharInstance(const Ch: StdChar): Boolean;

    function EndsWithWhitespaceAndEndOfLine: Boolean;

    function RetrieveIdentifier: StdString;
    function RetrieveNumericalValue(out Text: StdString): TNumericalValueType;
    function RetrieveGUIDValue: StdString;

    function RetrieveTextUntilEndOfLine: StdString;

    // Returns True only when the wanted instance was encountered (but not line breaks);
    function RetrieveTextUntilTextInstanceOrEndOfLine(const TextInstance: StdString;
      out RetrievedText: StdString): Boolean;

    procedure EstimateLineAndPosition(const ATextPos: Integer; out LinePos, HorizPos: Integer);

    function IsIdentifierLowCase(const Identifier: StdString): Boolean;
  public
    constructor Create;

    property SourceCode: StdString read FSourceCode write FSourceCode;
    property TextPos: Integer read FTextPos;
  end;

procedure AnnotationSkipWhitespace(const AnnotationCode: StdString; var TextPos: Integer);
function AnnotationRetrieveIdentifier(const AnnotationCode: StdString; var TextPos: Integer): StdString;

function CodeStartsWithText(const Code: StdString; const TextPos: Integer; const Text: StdString): Boolean;
function CodeStartsWithChar(const Code: StdString; const TextPos: Integer; const Ch: StdChar): Boolean;

implementation

uses
  SysUtils;

const
  WhitespaceChars = [#0, #9, #32];
  AllWhitespaceChars = [#0, #9, #10, #13, #32];
  LineBreakChars = [#10, #13];

  IdentifierFirstChars = ['A'..'Z', 'a'..'z', '_'];
  IdentifierFollowingChars = ['A'..'Z', 'a'..'z', '_', '0'..'9'];
  IdentifierUpperCaseChars = ['A'..'Z'];

  OctalChars = ['0'..'7'];
  DecimalChars = ['0'..'9'];
  HexadecimalChars = ['0'..'9', 'A'..'F', 'a'..'f'];

  GUIDChars = ['0'..'9', 'A'..'F', 'a'..'f', '-'];

  AnnotationIdentifierChars = ['A'..'Z', 'a'..'z'];

procedure AnnotationSkipWhitespace(const AnnotationCode: StdString; var TextPos: Integer);
begin
  while (TextPos <= Length(AnnotationCode)) and (AnnotationCode[TextPos] ='_') do
    Inc(TextPos);
end;

function AnnotationRetrieveIdentifier(const AnnotationCode: StdString; var TextPos: Integer): StdString;
var
  StartPos, CopyLen: Integer;
begin
  StartPos := TextPos;
  CopyLen := 0;

  while (TextPos <= Length(AnnotationCode)) and (AnnotationCode[TextPos] in AnnotationIdentifierChars) do
  begin
    Inc(CopyLen);
    Inc(TextPos);
  end;

  if CopyLen > 0 then
    Result := Copy(AnnotationCode, StartPos, CopyLen)
  else
    Result := '';
end;

function CodeStartsWithText(const Code: StdString; const TextPos: Integer; const Text: StdString): Boolean;
var
  I: Integer;
begin
  if 1 + Length(Code) - TextPos < Length(Text) then
    Exit(False);

  for I := 0 to Length(Text) - 1 do
    if Code[TextPos + I] <> Text[1 + I] then
      Exit(False);

  Result := True;
end;

function CodeStartsWithChar(const Code: StdString; const TextPos: Integer; const Ch: StdChar): Boolean;
begin
  Result := (TextPos <= Length(Code)) and (Code[TextPos] = Ch);
end;

constructor TCustomTextParser.Create;
begin
  inherited;

  FTextPos := 1;
end;

function TCustomTextParser.IsEndOfFile: Boolean;
begin
  Result := FTextPos > Length(FSourceCode);
end;

procedure TCustomTextParser.SkipWhitespace;
begin
  while (FTextPos <= Length(FSourceCode)) and (FSourceCode[FTextPos] in WhitespaceChars) do
    Inc(FTextPos);
end;

procedure TCustomTextParser.SkipWhitespaceAndLineBreaks;
begin
  while (FTextPos <= Length(FSourceCode)) and (FSourceCode[FTextPos] in AllWhitespaceChars) do
    Inc(FTextPos);
end;

procedure TCustomTextParser.SkipToNextLine;
begin
  while FTextPos <= Length(FSourceCode) do
  begin
    if FSourceCode[FTextPos] in LineBreakChars then
    begin
      Inc(FTextPos);

      if (FTextPos <= Length(FSourceCode)) and (FSourceCode[FTextPos] in LineBreakChars) then
        Inc(FTextPos);

      Break;
    end;

    Inc(FTextPos);
  end;
end;

procedure TCustomTextParser.Advance(const Delta: Integer);
begin
  Inc(FTextPos, Delta);
end;

function TCustomTextParser.StartsWithText(const Text: StdString): Boolean;
begin
  Result := CodeStartsWithText(FSourceCode, FTextPos, Text);
end;

function TCustomTextParser.StartsWithChar(const Ch: StdChar): Boolean;
begin
  Result := CodeStartsWithChar(FSourceCode, FTextPos, Ch);
end;

function TCustomTextParser.AdvanceWithText(const Text: StdString): Boolean;
begin
  Result := StartsWithText(Text);

  if Result then
    Inc(FTextPos, Length(Text));
end;

function TCustomTextParser.AdvanceWithChar(const Ch: StdChar): Boolean;
begin
  Result := StartsWithChar(Ch);

  if Result then
    Inc(FTextPos);
end;

function TCustomTextParser.SkipAfterNextTextInstance(const Text: StdString): Boolean;
begin
  while FTextPos <= Length(FSourceCode) do
  begin
    if StartsWithText(Text) then
    begin
      Inc(FTextPos, Length(Text));
      Exit(True);
    end;

    Inc(FTextPos);
  end;

  Result := False;
end;

function TCustomTextParser.SkipAfterNextCharInstance(const Ch: StdChar): Boolean;
begin
  while FTextPos <= Length(FSourceCode) do
  begin
    if FSourceCode[FTextPos] = Ch then
    begin
      Inc(FTextPos);
      Exit(True);
    end;

    Inc(FTextPos);
  end;

  Result := False;
end;

function TCustomTextParser.EndsWithWhitespaceAndEndOfLine: Boolean;
var
  Delta: Integer;
begin
  Delta := 0;

  while FTextPos + Delta <= Length(FSourceCode) do
  begin
    if FSourceCode[FTextPos + Delta] in LineBreakChars then
      Break;

    if not (FSourceCode[FTextPos + Delta] in WhitespaceChars) then
      Exit(False);

    Inc(Delta);
  end;

  Result := True;
end;

function TCustomTextParser.RetrieveIdentifier: StdString;
var
  StartPos, CopyLen: Integer;
begin
  Result := '';
  if (FTextPos > Length(FSourceCode)) or (not (FSourceCode[FTextPos] in IdentifierFirstChars)) then
    Exit;

  StartPos := FTextPos;
  CopyLen := 0;

  while (FTextPos <= Length(FSourceCode)) and (FSourceCode[FTextPos] in IdentifierFollowingChars) do
  begin
    Inc(CopyLen);
    Inc(FTextPos);
  end;

  Result := Copy(FSourceCode, StartPos, CopyLen);
end;

function TCustomTextParser.RetrieveNumericalValue(out Text: StdString): TNumericalValueType;
var
  StartPos, CopyLen: Integer;
  HexValue, OctalValue, NegativeValue, FoundDot, FoundExp: Boolean;
begin
  Text := '';
  Result := TNumericalValueType.None;

  if FTextPos > Length(FSourceCode) then
    Exit;

  // Check if value is Negative (starts with "-").
  if FSourceCode[FTextPos] = '-' then
  begin
    Inc(FTextPos);
    NegativeValue := True;
  end
  else
    NegativeValue := False;

  // Check if value is in Octal or Hexadecimal format.
  HexValue := False;
  OctalValue := False;

  if (FSourceCode[FTextPos] = '0') and (FTextPos < Length(FSourceCode) - 1) then
    if UpCase(FSourceCode[FTextPos + 1]) = 'X' then
    begin
      HexValue := True;
      Inc(FTextPos, 2); // Skip "0x" part.
    end
    else if FSourceCode[FTextPos + 1] in OctalChars then
    begin
      OctalValue := True;
      Inc(FTextPos); // Skip "0" part.
    end;

  // Parse the numerical value.
  StartPos := FTextPos;
  CopyLen := 0;

  FoundDot := False;
  FoundExp := False;

  if HexValue then
    while (FTextPos <= Length(FSourceCode)) and (FSourceCode[FTextPos] in HexadecimalChars) do
    begin
      Inc(CopyLen);
      Inc(FTextPos);
    end
  else if OctalValue then
    while (FTextPos <= Length(FSourceCode)) and (FSourceCode[FTextPos] in OctalChars) do
    begin
      Inc(CopyLen);
      Inc(FTextPos);
    end
  else
  begin
    while FTextPos <= Length(FSourceCode) do
    begin
      if (not FoundDot) and (not FoundExp) and (FTextPos < Length(FSourceCode)) and (FSourceCode[FTextPos] = '.') and
        (FSourceCode[FTextPos + 1] in DecimalChars) then
      begin
        FoundDot := True;

        // Skip both dot and digit now that they both have been validated.
        Inc(CopyLen, 2);
        Inc(FTextPos, 2);

        Continue;
      end
      else if (not FoundExp) and (FTextPos < Length(FSourceCode)) and (UpCase(FSourceCode[FTextPos]) = 'E') then
      begin
        FoundExp := True;

        // Skip "E" part.
        Inc(CopyLen);
        Inc(FTextPos);

        if (FTextPos < Length(FSourceCode)) and (FSourceCode[FTextPos] in ['+', '-']) and
          (FSourceCode[FTextPos + 1] in DecimalChars) then
        begin // Skip both operator and digit now that they both have been validated.
          Inc(CopyLen, 2);
          Inc(FTextPos, 2);
        end;

        Continue;
      end
      else if not (FSourceCode[FTextPos] in DecimalChars) then
        Break;

      Inc(CopyLen);
      Inc(FTextPos);
    end;
  end;

  if (CopyLen < 1) or OctalValue then
    Exit;

  // Skip format suffix.
  if FTextPos <= Length(FSourceCode) then
  begin
    if ((FoundDot or FoundExp) and (UpCase(FSourceCode[FTextPos]) = 'F')) or (UpCase(FSourceCode[FTextPos]) = 'L') then
      Inc(FTextPos)
    else if (FTextPos < Length(FSourceCode)) and (UpCase(FSourceCode[FTextPos]) = 'U') and
      (UpCase(FSourceCode[FTextPos + 1])  = 'L') then
      Inc(FTextPos, 2);
  end;

  Text := Copy(FSourceCode, StartPos, CopyLen);

  if HexValue then
  begin
    Text := '$' + Text;
    Result := TNumericalValueType.Hex;
  end
  else if FoundDot and (not FoundExp) then
    Result := TNumericalValueType.Float
  else if FoundExp then
    Result := TNumericalValueType.IEEEFloat
  else
    Result := TNumericalValueType.Integer;

  if NegativeValue then
    Text := '-' + Text;

  if HexValue or FoundExp then
    Text := UpperCase(Text);
end;

function TCustomTextParser.RetrieveGUIDValue: StdString;
var
  StartPos, CopyLen: Integer;
begin
  StartPos := FTextPos;
  CopyLen := 0;

  while (FTextPos <= Length(FSourceCode)) and (FSourceCode[FTextPos] in GUIDChars) do
  begin
    Inc(CopyLen);
    Inc(FTextPos);
  end;

  if CopyLen > 0 then
    Result := UpperCase(Copy(FSourceCode, StartPos, CopyLen))
  else
    Result := '';
end;

function TCustomTextParser.RetrieveTextUntilEndOfLine: StdString;
var
  StartPos, CopyLen: Integer;
begin
  StartPos := FTextPos;
  CopyLen := 0;

  while (FTextPos <= Length(FSourceCode)) and (not (FSourceCode[FTextPos] in LineBreakChars)) do
  begin
    Inc(CopyLen);
    Inc(FTextPos);
  end;

  if CopyLen > 0 then
  begin
    // If any text was retrieved, skip remaining line breaks.
    while (FTextPos <= Length(FSourceCode)) and (FSourceCode[FTextPos] in LineBreakChars) do
      Inc(FTextPos);

    Result := Copy(FSourceCode, StartPos, CopyLen);
  end
  else
    Result := '';
end;

function TCustomTextParser.RetrieveTextUntilTextInstanceOrEndOfLine(const TextInstance: StdString;
  out RetrievedText: StdString): Boolean;
var
  StartPos, CopyLen: Integer;
begin
  StartPos := FTextPos;
  CopyLen := 0;
  Result := False;

  // Skip initial line breaks, if any.
  while (FTextPos <= Length(FSourceCode)) and (FSourceCode[FTextPos] in LineBreakChars) do
    Inc(FTextPos);

  // Scan for text instance or line breaks.
  while (FTextPos <= Length(FSourceCode)) and (not (FSourceCode[FTextPos] in LineBreakChars)) do
  begin
    if StartsWithText(TextInstance) then
    begin
      Inc(FTextPos, Length(TextInstance));
      Result := True;
      Break;
    end;

    Inc(CopyLen);
    Inc(FTextPos);
  end;

  if CopyLen > 0 then
  begin
    // If any text was retrieved, skip remaining line breaks.
    while (FTextPos <= Length(FSourceCode)) and (FSourceCode[FTextPos] in LineBreakChars) do
      Inc(FTextPos);

    RetrievedText := Copy(FSourceCode, StartPos, CopyLen);
  end
  else
    RetrievedText := '';
end;

procedure TCustomTextParser.EstimateLineAndPosition(const ATextPos: Integer; out LinePos, HorizPos: Integer);
var
  CurPos: Integer;
begin
  LinePos := 1;
  HorizPos := 1;
  CurPos := 1;

  while (CurPos < ATextPos) and (CurPos <= Length(FSourceCode)) do
  begin
    if FSourceCode[CurPos] in LineBreakChars then
    begin
      // Skip second line break character, if exists.
      if (CurPos < Length(FSourceCode)) and (FSourceCode[CurPos + 1] in LineBreakChars) and
        (FSourceCode[CurPos] <> FSourceCode[CurPos + 1]) then
        Inc(CurPos);

      Inc(CurPos);

      Inc(LinePos);
      HorizPos := 1;

      Continue;
    end;

    Inc(CurPos);
    Inc(HorizPos);
  end;
end;

function TCustomTextParser.IsIdentifierLowCase(const Identifier: StdString): Boolean;
var
  I: Integer;
begin
  if Length(Identifier) < 1 then
    Exit(False);

  for I := 1 to Length(Identifier) do
    if Identifier[I] in IdentifierUpperCaseChars then
      Exit(False);

  Result := True;
end;

end.
