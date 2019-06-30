unit HPL.BasicTypes;
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
  SysUtils, HPL.Types;

type
  TParameterDeclaration = record
    Name: StdString;
    DataType: StdString;
    Annotation: StdString;
    PointerCount: Integer;
    ArraySizes: array of StdString;

    procedure Reset;
    procedure AddArraySize(const Value: StdString);
    function HasArraySizes: Boolean;
  end;

  TDataTypeAliasType = (None, Inverse);

  TDataTypeAlias = record
    Name: StdString;
    DataType: StdString;
    PointerIndex: Integer;
    AliasType: TDataTypeAliasType;
  end;

  TDataTypeAliases = class
  private
    FItems: array of TDataTypeAlias;

    FSearchList: array of Integer;
    FSearchListDirty: Boolean;

    function GetItem(const Index: Integer): TDataTypeAlias;
    function GetCount: Integer;

    procedure InitSearchList;
    procedure SwapSearchList(const Index1, Index2: Integer);
    function CompareSearchList(const Index1, Index2: Integer): Integer;
    function SplitSearchList(const Start, Stop: Integer): Integer;
    procedure SortSearchList(const Start, Stop: Integer);
    procedure UpdateSearchList;
  protected
    function IndexOf(const Name: StdString): Integer;
  public
    procedure Clear;
    procedure Remove(const Index: Integer);
    procedure Add(const Name, DataType: StdString; const PointerIndex: Integer; const AliasType: TDataTypeAliasType);
    procedure Define(const Name, DataType: StdString; const PointerIndex: Integer = 0;
      const AliasType: TDataTypeAliasType = TDataTypeAliasType.None);

    function Exists(const Name: StdString): Boolean;
    function GetAlias(const Name: StdString; out Alias: TDataTypeAlias): Boolean;
    procedure Resolve(var DataType: StdString; var PointerIndex: Integer);

    property Count: Integer read GetCount;
    property Items[const Index: Integer]: TDataTypeAlias read GetItem; default;
  end;

  EDataTypeAliasException = class(Exception);

resourcestring
  SDataTypeAliasIndexOutOfBounds = 'Data type alias index is out of bounds (%d).';

implementation

{$REGION 'TParameterDeclaration'}

procedure TParameterDeclaration.Reset;
begin
  FillChar(Self, SizeOf(TParameterDeclaration), 0);
end;

procedure TParameterDeclaration.AddArraySize(const Value: StdString);
var
  Index: Integer;
begin
  Index := Length(ArraySizes);
  SetLength(ArraySizes, Index + 1);
  ArraySizes[Index] := Value;
end;

function TParameterDeclaration.HasArraySizes: Boolean;
begin
  Result := Length(ArraySizes) > 0;
end;

{$ENDREGION}
{$REGION 'TDataTypeAliases'}

function TDataTypeAliases.GetCount: Integer;
begin
  Result := Length(FItems);
end;

function TDataTypeAliases.GetItem(const Index: Integer): TDataTypeAlias;
begin
  if (Index < 0) or (Index >= Length(FItems)) then
    raise EDataTypeAliasException.Create(Format(SDataTypeAliasIndexOutOfBounds, [Index]));

  Result := FItems[Index];
end;

procedure TDataTypeAliases.Clear;
begin
  SetLength(FItems, 0);
  SetLength(FSearchList, 0);
  FSearchListDirty := False;
end;

procedure TDataTypeAliases.Remove(const Index: Integer);
var
  I: Integer;
begin
  if (Index < 0) or (Index >= Length(FItems)) then
    Exit;

  for I := Index to Length(FItems) - 2 do
    FItems[I] := FItems[I + 1];

  SetLength(FItems, Length(FItems) - 1);
  FSearchListDirty := True;
end;

procedure TDataTypeAliases.InitSearchList;
var
  I: Integer;
begin
  if Length(FSearchList) <> Length(FItems) then
    SetLength(FSearchList, Length(FItems));

  for I := 0 to Length(FSearchList) - 1 do
    FSearchList[I] := I;
end;

procedure TDataTypeAliases.SwapSearchList(const Index1, Index2: Integer);
var
  TempValue: Integer;
begin
  TempValue := FSearchList[Index1];
  FSearchList[Index1] := FSearchList[Index2];
  FSearchList[Index2] := TempValue;
end;

function TDataTypeAliases.CompareSearchList(const Index1, Index2: Integer): Integer;
begin
  Result := CompareText(FItems[Index1].Name, FItems[Index2].Name);
end;

function TDataTypeAliases.SplitSearchList(const Start, Stop: Integer): Integer;
var
  Left, Right, Pivot: Integer;
begin
  Left := Start + 1;
  Right := Stop;
  Pivot := FSearchList[Start];

  while Left <= Right do
  begin
    while (Left <= Stop) and (CompareSearchList(FSearchList[Left], Pivot) < 0) do
      Inc(Left);

    while (Right > Start) and (CompareSearchList(FSearchList[Right], Pivot) >= 0) do
      Dec(Right);

    if Left < Right then
      SwapSearchList(Left, Right);
  end;

  SwapSearchList(Start, Right);
  Result := Right;
end;

procedure TDataTypeAliases.SortSearchList(const Start, Stop: Integer);
var
  SplitPt: Integer;
begin
  if Start < Stop then
  begin
    SplitPt := SplitSearchList(Start, Stop);

    SortSearchList(Start, SplitPt - 1);
    SortSearchList(SplitPt + 1, Stop);
  end;
end;

procedure TDataTypeAliases.UpdateSearchList;
begin
  InitSearchList;

  if Length(FSearchList) > 1 then
    SortSearchList(0, Length(FSearchList) - 1);

  FSearchListDirty := False;
end;

function TDataTypeAliases.IndexOf(const Name: StdString): Integer;
var
  Left, Right, Pivot, Res: Integer;
begin
  if FSearchListDirty then
    UpdateSearchList;

  Left := 0;
  Right := Length(FSearchList) - 1;

  while Left <= Right do
  begin
    Pivot := (Left + Right) div 2;
    Res := CompareText(FItems[FSearchList[Pivot]].Name, Name);

    if Res = 0 then
      Exit(FSearchList[Pivot]);

    if Res > 0 then
      Right := Pivot - 1
    else
      Left := Pivot + 1;
  end;

  Result := -1;
end;

procedure TDataTypeAliases.Add(const Name, DataType: StdString; const PointerIndex: Integer;
  const AliasType: TDataTypeAliasType);
var
  Index: Integer;
begin
  Index := Length(FItems);
  SetLength(FItems, Index + 1);

  FItems[Index].Name := Name;
  FItems[Index].DataType := DataType;
  FItems[Index].PointerIndex := PointerIndex;
  FItems[Index].AliasType := AliasType;

  FSearchListDirty := True;
end;

procedure TDataTypeAliases.Define(const Name, DataType: StdString; const PointerIndex: Integer;
  const AliasType: TDataTypeAliasType);
var
  Index: Integer;
begin
  Index := IndexOf(Name);
  if Index <> -1 then
    Remove(Index);

  Add(Name, DataType, PointerIndex, AliasType);
end;

function TDataTypeAliases.Exists(const Name: StdString): Boolean;
begin
  Result := IndexOf(Name) <> -1;
end;

function TDataTypeAliases.GetAlias(const Name: StdString; out Alias: TDataTypeAlias): Boolean;
var
  Index: Integer;
begin
  Index := IndexOf(Name);
  Result := Index <> -1;

  if Result then
    Alias := FItems[Index];
end;

procedure TDataTypeAliases.Resolve(var DataType: StdString; var PointerIndex: Integer);
var
  Alias: TDataTypeAlias;
begin
  while Length(DataType) > 0 do
  begin
    if not GetAlias(DataType, Alias) then
      Break;

    DataType := Alias.DataType;
    Inc(PointerIndex, Alias.PointerIndex);
  end;
end;

{$ENDREGION}

end.
