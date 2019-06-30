program HeaderParser;
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
{$ModeSwitch UnicodeStrings}

uses
  SysUtils, Classes,
  HPL.Annotations in '..\Sources\HPL.Annotations.pas',
  HPL.BasicTypes in '..\Sources\HPL.BasicTypes.pas',
  HPL.DataTypes in '..\Sources\HPL.DataTypes.pas',
  HPL.DefineSymbols in '..\Sources\HPL.DefineSymbols.pas',
  HPL.MethodParameters in '..\Sources\HPL.MethodParameters.pas',
  HPL.Parameters in '..\Sources\HPL.Parameters.pas',
  HPL.Parsers in '..\Sources\HPL.Parsers.pas',
  HPL.ReservedWords in '..\Sources\HPL.ReservedWords.pas',
  HPL.Types in '..\Sources\HPL.Types.pas';

function ReadAllText(const AFileName: string): string;
var
  LStream: TFileStream;
  LBuffer: TBytes;
  LBOMLength: Integer;
  LEncoding: TEncoding;
begin
  LStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(LBuffer, LStream.Size);
    LStream.ReadBuffer(LBuffer[0], Length(LBuffer));
    LEncoding := nil;
    LBOMLength := TEncoding.GetBufferEncoding(LBuffer, LEncoding);
    Result := LEncoding.GetString(LBuffer, LBOMLength, Length(LBuffer) - LBOMLength);
  finally
    LStream.Free;
  end;
end;

procedure WriteAllText(const AFileName, AContents: string);
var
  LStream: TFileStream;
  LBuffer: TBytes;
begin
  LStream := TFileStream.Create(AFileName, fmCreate or fmShareExclusive);
  try
    LBuffer := TEncoding.UTF8.GetBytes(AContents);
    LStream.WriteBuffer(LBuffer[0], Length(LBuffer));
  finally
    LStream.Free;
  end;
end;

var
  Parser: THeaderParser = nil;
  Source, Header, Output: string;

begin
  try
    if ParamCount < 3 then
    begin
      WriteLn('Header Parser syntax:');
      WriteLn('  headerparser.exe (file.h) (header.pas) (output.pas)');
      Exit;
    end;

    Source := ReadAllText(ParamStr(1));
    Header := ReadAllText(ParamStr(2));

    Parser := THeaderParser.Create([
//      TParserOption.LeaveComments,
//      TParserOption.LeaveUnsupportedSymbols,
      TParserOption.DefineCommonSymbols,
      TParserOption.GuessInterfaces,
      TParserOption.ShortenParameterNames,
      TParserOption.SeparateByType]);

    Parser.DebugOptions := [TParserDebugOption.StructParamSizes];
    try
        {Parser.Options := Parser.Options + [TParserOption.SkipOutput];
        ProcessFile('typedef.h');
        Parser.Options := Parser.Options - [TParserOption.SkipOutput];}


      Parser.SourceCode := Source;
      Parser.Process;

      Parser.CombineCode;
      Parser.DeclareObjectHandles;

      Output := Parser.FinalCode;

      Output := Header + SLineBreak + Output + SLineBreak + 'implementation' + SLineBreak + SLineBreak + 'end.';

      WriteAllText(ParamStr(3), Output);
    finally
      Parser.Free;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
