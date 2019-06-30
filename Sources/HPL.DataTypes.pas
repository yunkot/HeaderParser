unit HPL.DataTypes;
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
  TDataTypeGroup = (None, Fundamental, Structure, &Interface, Enum, Pointer, Callback);
  TDataTypeRelation = (None, System, &External, Redirect, Declared, &Forward);

  TDataTypeInfo = record
    Name: StdString;
    Size: Integer;
    Original: StdString;
    Translated: StdString;
    Group: TDataTypeGroup;
    Relation: TDataTypeRelation;
    PointerIndex: Integer;
  end;

  TDataTypes = class
  private
    FItems: array of TDataTypeInfo;

    FSearchList: array of Integer;
    FSearchListDirty: Boolean;

    procedure InitSearchList;
    procedure SwapSearchList(const Index1, Index2: Integer);
    function CompareSearchList(const Index1, Index2: Integer): Integer;
    function SplitSearchList(const Start, Stop: Integer): Integer;
    procedure SortSearchList(const Start, Stop: Integer);
    procedure UpdateSearchList;
  protected
    function IndexOf(const Name: StdString): Integer;
  public
    constructor Create;

    procedure AddCommonTypes;

    procedure Clear;
    procedure Remove(const Index: Integer);
    procedure Add(const Name, Original, Translated: StdString; const Size, PointerIndex: Integer;
      const Group: TDataTypeGroup; const Relation: TDataTypeRelation);
    procedure Define(const Name, Original, Translated: StdString; const Size, PointerIndex: Integer;
      const Group: TDataTypeGroup; const Relation: TDataTypeRelation);

    function Exists(const Name: StdString): Boolean;
    function GetInfo(const Name: StdString; out Info: TDataTypeInfo): Boolean;
    function Resolve(const Name: StdString; out Info: TDataTypeInfo): Boolean;
  end;

const
  DefaultPointerDataTypeSize = 8;

implementation

uses
  SysUtils;

{$REGION 'Global Constants'}

type
  TDataTypeEntry = record
    Name: StdString;
    Size: Integer;
    Translated: StdString;
  end;

const
  CommonDataTypes: array[0..76] of TDataTypeEntry = (
    (Name: 'BYTE'; Size: 1; Translated: 'Byte'),
    (Name: 'UINT8'; Size: 1; Translated: 'Byte'),
    (Name: 'UINT8_T'; Size: 1; Translated: 'Byte'),
    (Name: 'char'; Size: 1; Translated: 'AnsiChar'),
    (Name: 'unsigned char'; Size: 1; Translated: 'Byte'),

    (Name: 'INT8'; Size: 1; Translated: 'ShortInt'),
    (Name: 'INT8_T'; Size: 1; Translated: 'ShortInt'),
    (Name: 'signed char'; Size: 1; Translated: 'ShortInt'),

    (Name: 'INT16'; Size: 2; Translated: 'SmallInt'),
    (Name: 'INT16_T'; Size: 2; Translated: 'SmallInt'),
    (Name: 'short'; Size: 2; Translated: 'SmallInt'),
    (Name: 'short int'; Size: 2; Translated: 'SmallInt'),
    (Name: 'signed short'; Size: 2; Translated: 'SmallInt'),
    (Name: 'signed short int'; Size: 2; Translated: 'SmallInt'),

    (Name: 'UINT16'; Size: 2; Translated: 'Word'),
    (Name: 'UINT16_T'; Size: 2; Translated: 'Word'),
    (Name: 'USHORT'; Size: 2; Translated: 'Word'),
    (Name: 'WORD'; Size: 2; Translated: 'Word'),
    (Name: 'unsigned short'; Size: 2; Translated: 'Word'),
    (Name: 'unsigned short int'; Size: 2; Translated: 'Word'),

    (Name: 'INT32'; Size: 4; Translated: 'Integer'),
    (Name: 'INT32_T'; Size: 4; Translated: 'Integer'),
    (Name: 'LONG32'; Size: 4; Translated: 'Integer'),
    (Name: 'int'; Size: 4; Translated: 'Integer'),
    (Name: 'signed int'; Size: 4; Translated: 'Integer'),
    (Name: 'long'; Size: 4; Translated: 'Integer'),
    (Name: 'long int'; Size: 4; Translated: 'Integer'),
    (Name: 'signed long'; Size: 4; Translated: 'Integer'),
    (Name: 'signed long int'; Size: 4; Translated: 'Integer'),

    (Name: 'UINT'; Size: 4; Translated: 'Cardinal'),
    (Name: 'UINT32'; Size: 4; Translated: 'Cardinal'),
    (Name: 'UINT32_T'; Size: 4; Translated: 'Cardinal'),
    (Name: 'ULONG'; Size: 4; Translated: 'Cardinal'),
    (Name: 'ULONG32'; Size: 4; Translated: 'Cardinal'),
    (Name: 'DWORD'; Size: 4; Translated: 'Cardinal'),
    (Name: 'unsigned'; Size: 4; Translated: 'Cardinal'),
    (Name: 'unsigned int'; Size: 4; Translated: 'Cardinal'),
    (Name: 'unsigned long'; Size: 4; Translated: 'Cardinal'),
    (Name: 'unsigned long int'; Size: 4; Translated: 'Cardinal'),

    (Name: 'INT64'; Size: 8; Translated: 'Int64'),
    (Name: 'INT64_T'; Size: 8; Translated: 'Int64'),
    (Name: 'LONG64'; Size: 8; Translated: 'Int64'),
    (Name: 'LARGE_INTEGER'; Size: 8; Translated: 'Int64'),
    (Name: 'long long'; Size: 8; Translated: 'Int64'),
    (Name: 'long long int'; Size: 8; Translated: 'Int64'),
    (Name: 'long long int'; Size: 8; Translated: 'Int64'),
    (Name: 'signed long long'; Size: 8; Translated: 'Int64'),
    (Name: 'signed long long int'; Size: 8; Translated: 'Int64'),

    (Name: 'UINT64'; Size: 8; Translated: 'UInt64'),
    (Name: 'UINT64_T'; Size: 8; Translated: 'UInt64'),
    (Name: 'ULONG64'; Size: 8; Translated: 'UInt64'),
    (Name: 'DWORD64'; Size: 8; Translated: 'UInt64'),
    (Name: 'QWORD'; Size: 8; Translated: 'UInt64'),
    (Name: 'unsigned long long'; Size: 8; Translated: 'UInt64'),
    (Name: 'unsigned long long int'; Size: 8; Translated: 'UInt64'),

    (Name: 'BOOL'; Size: 4; Translated: 'LongBool'),
    (Name: 'BOOLEAN'; Size: 1; Translated: 'ByteBool'),

    (Name: 'float'; Size: 4; Translated: 'Single'),
    (Name: 'double'; Size: 8; Translated: 'Double'),
    (Name: 'long double'; Size: 8; Translated: 'Double'),

    (Name: 'WCHAR'; Size: 2; Translated: 'WideChar'),

    (Name: 'LPSTR'; Size: DefaultPointerDataTypeSize; Translated: 'PAnsiChar'),
    (Name: 'LPCSTR'; Size: DefaultPointerDataTypeSize; Translated: 'PAnsiChar'),

    (Name: 'LPWSTR'; Size: DefaultPointerDataTypeSize; Translated: 'PWideChar'),
    (Name: 'LPCWSTR'; Size: DefaultPointerDataTypeSize; Translated: 'PWideChar'),
    (Name: 'LMSTR'; Size: DefaultPointerDataTypeSize; Translated: 'PWideChar'),
    (Name: 'LMCSTR'; Size: DefaultPointerDataTypeSize; Translated: 'PWideChar'),

    (Name: 'LPCVOID'; Size: DefaultPointerDataTypeSize; Translated: 'Pointer'),
    (Name: 'LPVOID'; Size: DefaultPointerDataTypeSize; Translated: 'Pointer'),
    (Name: 'HANDLE'; Size: DefaultPointerDataTypeSize; Translated: 'THandle'),
    (Name: 'HMONITOR'; Size: DefaultPointerDataTypeSize; Translated: 'HMonitor'),
    (Name: 'HWND'; Size: DefaultPointerDataTypeSize; Translated: 'HWND'),
    (Name: 'SIZE_T'; Size: DefaultPointerDataTypeSize; Translated: 'SIZE_T'),

    (Name: 'HRESULT'; Size: 4; Translated: 'HResult'),
    (Name: 'void'; Size: DefaultPointerDataTypeSize; Translated: 'Pointer'),

    (Name: 'uintptr_t'; Size: 4; Translated: 'SizeUInt'),
    (Name: 'intptr_t'; Size: 4; Translated: 'SizeInt')
  );

{$ENDREGION}
{$REGION 'TDataTypes'}

constructor TDataTypes.Create;
begin
  inherited;

  AddCommonTypes;
end;

procedure TDataTypes.Clear;
begin
  SetLength(FItems, 0);
  SetLength(FSearchList, 0);
  FSearchListDirty := False;
end;

procedure TDataTypes.Remove(const Index: Integer);
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

procedure TDataTypes.InitSearchList;
var
  I: Integer;
begin
  if Length(FSearchList) <> Length(FItems) then
    SetLength(FSearchList, Length(FItems));

  for I := 0 to Length(FSearchList) - 1 do
    FSearchList[I] := I;
end;

procedure TDataTypes.SwapSearchList(const Index1, Index2: Integer);
var
  TempValue: Integer;
begin
  TempValue := FSearchList[Index1];
  FSearchList[Index1] := FSearchList[Index2];
  FSearchList[Index2] := TempValue;
end;

function TDataTypes.CompareSearchList(const Index1, Index2: Integer): Integer;
begin
  Result := CompareText(FItems[Index1].Name, FItems[Index2].Name);
end;

function TDataTypes.SplitSearchList(const Start, Stop: Integer): Integer;
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

procedure TDataTypes.SortSearchList(const Start, Stop: Integer);
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

procedure TDataTypes.UpdateSearchList;
begin
  InitSearchList;

  if Length(FSearchList) > 1 then
    SortSearchList(0, Length(FSearchList) - 1);

  FSearchListDirty := False;
end;

function TDataTypes.IndexOf(const Name: StdString): Integer;
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

procedure TDataTypes.Add(const Name, Original, Translated: StdString; const Size, PointerIndex: Integer;
  const Group: TDataTypeGroup; const Relation: TDataTypeRelation);
var
  Index: Integer;
begin
  Index := Length(FItems);
  SetLength(FItems, Index + 1);

  FItems[Index].Name := Name;
  FItems[Index].Original := Original;
  FItems[Index].Translated := Translated;
  FItems[Index].Size := Size;
  FItems[Index].Group := Group;
  FItems[Index].Relation := Relation;
  FItems[Index].PointerIndex := PointerIndex;

  FSearchListDirty := True;
end;

procedure TDataTypes.Define(const Name, Original, Translated: StdString; const Size, PointerIndex: Integer;
  const Group: TDataTypeGroup; const Relation: TDataTypeRelation);
var
  Index: Integer;
begin
  Index := IndexOf(Name);
  if Index <> -1 then
    Remove(Index);

  Add(Name, Original, Translated, Size, PointerIndex, Group, Relation);
end;

procedure TDataTypes.AddCommonTypes;
var
  I, BaseIndex: Integer;
begin
  BaseIndex := Length(FItems);
  SetLength(FItems, BaseIndex + High(CommonDataTypes) + 4);

  for I := 0 to High(CommonDataTypes) do
  begin
    FItems[BaseIndex + I].Name := CommonDataTypes[I].Name;
    FItems[BaseIndex + I].Size := CommonDataTypes[I].Size;
    FItems[BaseIndex + I].Translated := CommonDataTypes[I].Translated;

    FItems[BaseIndex + I].Original := '';
    FItems[BaseIndex + I].Group := TDataTypeGroup.Fundamental;
    FItems[BaseIndex + I].Relation := TDataTypeRelation.System;
  end;

  FSearchListDirty := True;
end;

function TDataTypes.Exists(const Name: StdString): Boolean;
begin
  Result := IndexOf(Name) <> -1;
end;

function TDataTypes.GetInfo(const Name: StdString; out Info: TDataTypeInfo): Boolean;
var
  Index: Integer;
begin
  Index := IndexOf(Name);
  Result := Index <> -1;

  if Result then
    Info := FItems[Index];
end;

function TDataTypes.Resolve(const Name: StdString; out Info: TDataTypeInfo): Boolean;
var
  LinkName: StdString;
begin
  LinkName := Name;
  Result := False;

  while Length(LinkName) > 0 do
  begin
    if not GetInfo(LinkName, Info) then
      Exit;

    if (Info.Relation = TDataTypeRelation.Redirect) and (Length(Info.Original) > 0) then
      LinkName := Info.Original
    else
      LinkName := '';

    Result := True;
  end;
end;

{$ENDREGION}

end.
