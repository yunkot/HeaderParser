unit HPL.MethodParameters;
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
  TMethodParameter = class
  private
    FName: StdString;
    FDataType: StdString;
    FProcessedDataType: StdString;
    FAccessModifier: StdString;
  public
    constructor Create(const AName, ADataType, AProcessedDataType, AAccessModifier: StdString);

    property Name: StdString read FName;
    property DataType: StdString read FDataType;
    property ProcessedDataType: StdString read FProcessedDataType write FProcessedDataType;
    property AccessModifier: StdString read FAccessModifier write FAccessModifier;
  end;

  TMethodParameters = class
  private
    FItems: array of TMethodParameter;

    function GetCount: Integer;
    function GetItem(const Index: Integer): TMethodParameter;
  public
    destructor Destroy; override;

    procedure Clear;
    function Add(const Item: TMethodParameter): Integer;

    property Count: Integer read GetCount;
    property Items[const Index: Integer]: TMethodParameter read GetItem; default;
  end;

implementation

uses
  SysUtils;

{$REGION 'TMethodParameter'}

constructor TMethodParameter.Create(const AName, ADataType, AProcessedDataType, AAccessModifier: StdString);
begin
  inherited Create;

  FName := AName;
  FDataType := ADataType;
  FProcessedDataType := AProcessedDataType;
  FAccessModifier := AAccessModifier;
end;

{$ENDREGION}
{$REGION 'TMethodParameters'}

destructor TMethodParameters.Destroy;
begin
  Clear;

  inherited;
end;

function TMethodParameters.GetCount: Integer;
begin
  Result := Length(FItems);
end;

function TMethodParameters.GetItem(const Index: Integer): TMethodParameter;
begin
  if (Index >= 0) and (Index < Length(FItems)) then
    Result := FItems[Index]
  else
    Result := nil;
end;

procedure TMethodParameters.Clear;
var
  I: Integer;
begin
  for I := Length(FItems) - 1 downto 0 do
    FItems[I].Free;

  SetLength(FItems, 0);
end;

function TMethodParameters.Add(const Item: TMethodParameter): Integer;
begin
  Result := Length(FItems);
  SetLength(FItems, Result + 1);

  FItems[Result] := Item;
end;

{$ENDREGION}

end.
