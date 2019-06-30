unit HPL.ReservedWords;
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

function IsReservedWord(const Name: StdString): Boolean;

implementation

uses
  SysUtils;

const
  ReservedWords: array[0..81] of StdString = ('absolute', 'and', 'array', 'as', 'asm', 'at', 'automated', 'begin',
    'case', 'class', 'const', 'constructor', 'dc', 'destructor', 'dispinterface', 'dispose', 'div', 'do', 'downto',
    'else', 'end', 'except', 'exit', 'exports', 'false', 'file', 'finalization', 'finally', 'for', 'function', 'goto',
    'hdc', 'if', 'implementation', 'in', 'inherited', 'initialization', 'inline', 'interface', 'is', 'label',
    'library', 'mod', 'new', 'nil', 'not', 'object', 'of', 'on', 'operator', 'or', 'packed', 'private', 'procedure',
    'program', 'property', 'protected', 'public', 'published', 'raise', 'record', 'reintroduce', 'repeat',
    'resourcestring', 'self', 'set', 'shl', 'shr', 'string', 'then', 'threadvar', 'to', 'true', 'try', 'type', 'unit',
    'until', 'uses', 'var', 'while', 'with', 'xor');

function IsReservedWord(const Name: StdString): Boolean;
var
  Left, Right, Pivot, Res: Integer;
begin
  Left := 0;
  Right := High(ReservedWords);

  while Left <= Right do
  begin
    Pivot := (Left + Right) div 2;
    Res := CompareText(ReservedWords[Pivot], Name);

    if Res = 0 then
      Exit(True);

    if Res > 0 then
      Right := Pivot - 1
    else
      Left := Pivot + 1;
  end;

  Result := False;
end;

end.
