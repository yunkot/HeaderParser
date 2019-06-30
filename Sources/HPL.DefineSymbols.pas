unit HPL.DefineSymbols;
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
  PDefineSymbol = ^TDefineSymbol;
  TDefineSymbol = record
    SymbolName: StdString;
    SymbolValue: Integer;
  end;

  TDefineSymbols = class
  private
    FItems: array of TDefineSymbol;

    FSearchList: array of Integer;
    FSearchListDirty: Boolean;

    function GetCount: Integer;
    function GetItem(const Index: Integer): PDefineSymbol;

    procedure InitSearchList;
    procedure SwapSearchList(const Index1, Index2: Integer);
    function CompareSearchList(const Index1, Index2: Integer): Integer;
    function SplitSearchList(const Start, Stop: Integer): Integer;
    procedure SortSearchList(const Start, Stop: Integer);
    procedure UpdateSearchList;
  public
    procedure Clear;
    function Add(const SymbolName: StdString): Integer;
    procedure Remove(const Index: Integer);
    function IndexOf(const SymbolName: StdString): Integer;

    function Define(const SymbolName: StdString; const SymbolValue: Integer = 0): Integer;

    property Count: Integer read GetCount;
    property Items[const Index: Integer]: PDefineSymbol read GetItem; default;
  end;

implementation

uses
  SysUtils;

function TDefineSymbols.GetCount: Integer;
begin
  Result := Length(FItems);
end;

function TDefineSymbols.GetItem(const Index: Integer): PDefineSymbol;
begin
  if (Index >= 0) and (Index < Length(FItems)) then
    Result := @FItems[Index]
  else
    Result := nil;
end;

procedure TDefineSymbols.Clear;
begin
  SetLength(FItems, 0);
  SetLength(FSearchList, 0);
  FSearchListDirty := False;
end;

function TDefineSymbols.Add(const SymbolName: StdString): Integer;
begin
  Result := Length(FItems);
  SetLength(FItems, Result + 1);

  FItems[Result].SymbolName := SymbolName;
  FItems[Result].SymbolValue := 0;

  FSearchListDirty := True;
end;

procedure TDefineSymbols.Remove(const Index: Integer);
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

procedure TDefineSymbols.InitSearchList;
var
  I: Integer;
begin
  if Length(FSearchList) <> Length(FItems) then
    SetLength(FSearchList, Length(FItems));

  for I := 0 to Length(FSearchList) - 1 do
    FSearchList[I] := I;
end;

procedure TDefineSymbols.SwapSearchList(const Index1, Index2: Integer);
var
  TempValue: Integer;
begin
  TempValue := FSearchList[Index1];
  FSearchList[Index1] := FSearchList[Index2];
  FSearchList[Index2] := TempValue;
end;

function TDefineSymbols.CompareSearchList(const Index1, Index2: Integer): Integer;
begin
  Result := CompareStr(FItems[Index1].SymbolName, FItems[Index2].SymbolName);
end;

function TDefineSymbols.SplitSearchList(const Start, Stop: Integer): Integer;
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

procedure TDefineSymbols.SortSearchList(const Start, Stop: Integer);
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

procedure TDefineSymbols.UpdateSearchList;
begin
  InitSearchList;

  if Length(FSearchList) > 1 then
    SortSearchList(0, Length(FSearchList) - 1);

  FSearchListDirty := False;
end;

function TDefineSymbols.IndexOf(const SymbolName: StdString): Integer;
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
    Res := CompareStr(FItems[FSearchList[Pivot]].SymbolName, SymbolName);

    if Res = 0 then
      Exit(FSearchList[Pivot]);

    if Res > 0 then
      Right := Pivot - 1
    else
      Left := Pivot + 1;
  end;

  Result := -1;
end;

function TDefineSymbols.Define(const SymbolName: StdString; const SymbolValue: Integer): Integer;
begin
  Result := IndexOf(SymbolName);
  if Result = -1 then
    Result := Add(SymbolName);

  FItems[Result].SymbolValue := SymbolValue;
end;

end.
