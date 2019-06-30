unit HPL.Parameters;
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
  HPL.Types;

type
  TCustomParameter = class
  private
    FOffset: Integer;
  protected
    function GetSize: Integer; virtual;
  public
    constructor Create(const AOffset: Integer);

    property Offset: Integer read FOffset;
    property Size: Integer read GetSize;
  end;

  TNormalParameter = class(TCustomParameter)
  private
    FName: StdString;
    FDataType: StdString;
    FDataTypeSize: Integer;
    FTranslatedType: StdString;
    FPointerIndex: Integer;
    FArrays: array of StdString;

    function GetArrayCount: Integer;
    function GetArray(const Index: Integer): StdString;
  protected
    function GetSize: Integer; override;
  public
    constructor Create(const AName, ADataType, ATranslatedType: StdString; const ADataTypeSize, AOffset,
      APointerIndex: Integer);

    function AddArray(const Value: StdString): Integer;

    property Name: StdString read FName;
    property DataType: StdString read FDataType;
    property DataTypeSize: Integer read FDataTypeSize;
    property PointerIndex: Integer read FPointerIndex;
    property TranslatedType: StdString read FTranslatedType write FTranslatedType;

    property ArrayCount: Integer read GetArrayCount;
    property Arrays[const Index: Integer]: StdString read GetArray;
  end;

  TUnionParameter = class(TCustomParameter)
  private
    FElements: array of TCustomParameter;

    function GetCount: Integer;
    function GetElement(const Index: Integer): TCustomParameter;
  protected
    function GetSize: Integer; override;
  public
    destructor Destroy; override;

    procedure Clear;
    function Add(const Element: TCustomParameter): Integer;

    property Count: Integer read GetCount;
    property Elements[const Index: Integer]: TCustomParameter read GetElement; default;
  end;

  TStructParameter = class(TCustomParameter)
  private
    FElements: array of TCustomParameter;

    function GetCount: Integer;
    function GetElement(const Index: Integer): TCustomParameter;
  protected
    function GetSize: Integer; override;
  public
    destructor Destroy; override;

    procedure Clear;
    function Add(const Element: TCustomParameter): Integer;

    property Count: Integer read GetCount;
    property Elements[const Index: Integer]: TCustomParameter read GetElement; default;
  end;

  TParameters = class
  private
    FItems: array of TCustomParameter;

    function GetCount: Integer;
    function GetItem(const Index: Integer): TCustomParameter;
  public
    destructor Destroy; override;

    procedure Clear;
    function Add(const Item: TCustomParameter): Integer;

    property Count: Integer read GetCount;
    property Items[const Index: Integer]: TCustomParameter read GetItem; default;
  end;

  TParameterHolder = class
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF} FParameter: TCustomParameter;
    FChildren: array of TParameterHolder;
    FOwnsChildren: Boolean;

    function GetChild(const Index: Integer): TParameterHolder;
    function GetChildCount: Integer;
  public
    constructor Create(const AParameter: TCustomParameter = nil; const AOwnsChildren: Boolean = True);
    destructor Destroy; override;

    procedure Clear;
    function Add(const AChild: TParameterHolder): Integer;

    property OwnsChildren: Boolean read FOwnsChildren;

    property Parameter: TCustomParameter read FParameter;

    property ChildCount: Integer read GetChildCount;
    property Child[const Index: Integer]: TParameterHolder read GetChild; default;
  end;

implementation

uses
  SysUtils;

{$REGION 'Global Constants'}

const
  DefaultArraySizeCount = 128;

{$ENDREGION}
{$REGION 'TCustomParameter'}

constructor TCustomParameter.Create(const AOffset: Integer);
begin
  inherited Create;

  FOffset := AOffset;
end;

function TCustomParameter.GetSize: Integer;
begin
  Result := 0;
end;

{$ENDREGION}
{$REGION 'TNormalParameter'}

constructor TNormalParameter.Create(const AName, ADataType, ATranslatedType: StdString; const ADataTypeSize, AOffset,
  APointerIndex: Integer);
begin
  inherited Create(AOffset);

  FName := AName;
  FDataType := ADataType;
  FDataTypeSize := ADataTypeSize;
  FTranslatedType := ATranslatedType;
  FPointerIndex := APointerIndex;
end;

function TNormalParameter.GetArrayCount: Integer;
begin
  Result := Length(FArrays);
end;

function TNormalParameter.GetArray(const Index: Integer): StdString;
begin
  if (Index >= 0) and (Index < Length(FArrays)) then
    Result := FArrays[Index]
  else
    Result := '';
end;

function TNormalParameter.AddArray(const Value: StdString): Integer;
begin
  Result := Length(FArrays);
  SetLength(FArrays, Result + 1);

  FArrays[Result] := Value;
end;

function TNormalParameter.GetSize: Integer;
var
  I: Integer;
begin
  Result := FDataTypeSize;

  for I := 0 to Length(FArrays) - 1 do
    if Length(FArrays[I]) > 0 then
      Result := Result * StrToIntDef(FArrays[I], DefaultArraySizeCount);
end;

{$ENDREGION}
{$REGION 'TUnionParameter'}

destructor TUnionParameter.Destroy;
begin
  Clear;

  inherited;
end;

function TUnionParameter.GetCount: Integer;
begin
  Result := Length(FElements);
end;

function TUnionParameter.GetElement(const Index: Integer): TCustomParameter;
begin
  if (Index >= 0) and (Index < Length(FElements)) then
    Result := FElements[Index]
  else
    Result := nil;
end;

procedure TUnionParameter.Clear;
var
  I: Integer;
begin
  for I := Length(FElements) - 1 downto 0 do
    FElements[I].Free;

  SetLength(FElements, 0);
end;

function TUnionParameter.Add(const Element: TCustomParameter): Integer;
begin
  Result := Length(FElements);
  SetLength(FElements, Result + 1);

  FElements[Result] := Element;
end;

function TUnionParameter.GetSize: Integer;
var
  I, ElementSize: Integer;
begin
  Result := 0;

  for I := 0 to Length(FElements) - 1 do
  begin
    ElementSize := FElements[I].Size;
    if ElementSize > Result then
      Result := ElementSize;
  end;
end;

{$ENDREGION}
{$REGION 'TStructParameter'}

destructor TStructParameter.Destroy;
begin
  Clear;

  inherited;
end;

function TStructParameter.GetCount: Integer;
begin
  Result := Length(FElements);
end;

function TStructParameter.GetElement(const Index: Integer): TCustomParameter;
begin
  if (Index >= 0) and (Index < Length(FElements)) then
    Result := FElements[Index]
  else
    Result := nil;
end;

procedure TStructParameter.Clear;
var
  I: Integer;
begin
  for I := Length(FElements) - 1 downto 0 do
    FElements[I].Free;

  SetLength(FElements, 0);
end;

function TStructParameter.Add(const Element: TCustomParameter): Integer;
begin
  Result := Length(FElements);
  SetLength(FElements, Result + 1);

  FElements[Result] := Element;
end;

function TStructParameter.GetSize: Integer;
var
  I: Integer;
begin
  Result := 0;

  for I := 0 to Length(FElements) - 1 do
    Inc(Result, FElements[I].Size);
end;

{$ENDREGION}
{$REGION 'TParameters'}

destructor TParameters.Destroy;
begin
  Clear;

  inherited;
end;

function TParameters.GetCount: Integer;
begin
  Result := Length(FItems);
end;

function TParameters.GetItem(const Index: Integer): TCustomParameter;
begin
  if (Index >= 0) and (Index < Length(FItems)) then
    Result := FItems[Index]
  else
    Result := nil;
end;

procedure TParameters.Clear;
var
  I: Integer;
begin
  for I := Length(FItems) - 1 downto 0 do
    FItems[I].Free;

  SetLength(FItems, 0);
end;

function TParameters.Add(const Item: TCustomParameter): Integer;
begin
  Result := Length(FItems);
  SetLength(FItems, Result + 1);

  FItems[Result] := Item;
end;

{$ENDREGION}
{$REGION 'TParameterHolder'}

constructor TParameterHolder.Create(const AParameter: TCustomParameter; const AOwnsChildren: Boolean);
begin
  inherited Create;

  FParameter := AParameter;
  FOwnsChildren := AOwnsChildren;
end;

destructor TParameterHolder.Destroy;
begin
  Clear;

  inherited;
end;

procedure TParameterHolder.Clear;
var
  I: Integer;
begin
  if FOwnsChildren then
    for I := Length(FChildren) - 1 downto 0 do
      FChildren[I].Free;

  SetLength(FChildren, 0);
end;

function TParameterHolder.GetChildCount: Integer;
begin
  Result := Length(FChildren);
end;

function TParameterHolder.GetChild(const Index: Integer): TParameterHolder;
begin
  if (Index >= 0) and (Index < Length(FChildren)) then
    Result := FChildren[Index]
  else
    Result := nil;
end;

function TParameterHolder.Add(const AChild: TParameterHolder): Integer;
begin
  Result := Length(FChildren);
  SetLength(FChildren, Result + 1);

  FChildren[Result] := AChild;
end;

{$ENDREGION}

end.
