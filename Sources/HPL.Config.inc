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
{$SCOPEDENUMS ON}
{$B-}
{$I-}

{$IFDEF FPC}
  // FreePascal compiler directives.

  {$MODE DELPHI}
  {$MODESWITCH TYPEHELPERS}
  {$ModeSwitch UnicodeStrings}

  {$IFDEF CPU32}
    {$IFDEF I386}
      {$DEFINE CPUX86}
    {$ENDIF}
  {$ENDIF}

  {$IFDEF CPU64}
    {$DEFINE CPUX64}
    {$DEFINE PUREPASCAL}
  {$ENDIF}

  {$IFDEF MSDOS}
    {$DEFINE PXL_COMPACT}
  {$ENDIF}

{$ELSE}
  // Delphi compiler directives.

  {$DEFINE DELPHI}

  {$IFDEF VER180} // Delphi 2006
    {$DEFINE DELPHI_LEGACY}
    {$DEFINE DELPHI_2006}
  {$ENDIF}

  {$IFDEF VER185} // Delphi 2007
    {$DEFINE DELPHI_LEGACY}
    {$DEFINE DELPHI_2007}
  {$ENDIF}

  {$IFDEF VER200} // Delphi 2009
    {$DEFINE DELPHI_2009_UP}
    {$DEFINE DELPHI_2009}
  {$ENDIF}

  {$IFDEF VER210} // Delphi 2010
    {$DEFINE DELPHI_2009_UP}
    {$DEFINE DELPHI_2010_UP}
    {$DEFINE DELPHI_2010}
  {$ENDIF}

  {$IFDEF VER220} // Delphi XE
    {$DEFINE DELPHI_2009_UP}
    {$DEFINE DELPHI_2010_UP}
    {$DEFINE DELPHI_XE_UP}
    {$DEFINE DELPHI_XE}
  {$ENDIF}

  {$IFDEF VER230} // Delphi XE 2
    {$DEFINE DELPHI_2009_UP}
    {$DEFINE DELPHI_2010_UP}
    {$DEFINE DELPHI_XE_UP}
    {$DEFINE DELPHI_XE2_UP}
    {$DEFINE DELPHI_XE2}
  {$ENDIF}

  {$IFDEF VER240} // Delphi XE 3
    {$DEFINE DELPHI_2009_UP}
    {$DEFINE DELPHI_2010_UP}
    {$DEFINE DELPHI_XE_UP}
    {$DEFINE DELPHI_XE2_UP}
    {$DEFINE DELPHI_XE3_UP}
    {$DEFINE DELPHI_XE3}
  {$ENDIF}

  {$IFDEF VER250} // Delphi XE 4
    {$DEFINE DELPHI_2009_UP}
    {$DEFINE DELPHI_2010_UP}
    {$DEFINE DELPHI_XE_UP}
    {$DEFINE DELPHI_XE2_UP}
    {$DEFINE DELPHI_XE3_UP}
    {$DEFINE DELPHI_XE4_UP}
    {$DEFINE DELPHI_XE4}
  {$ENDIF}

  {$IFDEF VER260} // Delphi XE 5
    {$DEFINE DELPHI_2009_UP}
    {$DEFINE DELPHI_2010_UP}
    {$DEFINE DELPHI_XE_UP}
    {$DEFINE DELPHI_XE2_UP}
    {$DEFINE DELPHI_XE3_UP}
    {$DEFINE DELPHI_XE4_UP}
    {$DEFINE DELPHI_XE5_UP}
    {$DEFINE DELPHI_XE5}
  {$ENDIF}

  {$IFDEF VER270} // Delphi XE 6
    {$DEFINE DELPHI_2009_UP}
    {$DEFINE DELPHI_2010_UP}
    {$DEFINE DELPHI_XE_UP}
    {$DEFINE DELPHI_XE2_UP}
    {$DEFINE DELPHI_XE3_UP}
    {$DEFINE DELPHI_XE4_UP}
    {$DEFINE DELPHI_XE5_UP}
    {$DEFINE DELPHI_XE5}
    {$DEFINE DELPHI_XE6_UP}
    {$DEFINE DELPHI_XE6}
  {$ENDIF}

  {$IFDEF VER280} // Delphi XE 7
    {$DEFINE DELPHI_2009_UP}
    {$DEFINE DELPHI_2010_UP}
    {$DEFINE DELPHI_XE_UP}
    {$DEFINE DELPHI_XE2_UP}
    {$DEFINE DELPHI_XE3_UP}
    {$DEFINE DELPHI_XE4_UP}
    {$DEFINE DELPHI_XE5_UP}
    {$DEFINE DELPHI_XE5}
    {$DEFINE DELPHI_XE6_UP}
    {$DEFINE DELPHI_XE6}
    {$DEFINE DELPHI_XE7_UP}
    {$DEFINE DELPHI_XE7}
  {$ENDIF}

  {$IFDEF VER290} // Delphi XE 8 ?
    {$DEFINE DELPHI_2009_UP}
    {$DEFINE DELPHI_2010_UP}
    {$DEFINE DELPHI_XE_UP}
    {$DEFINE DELPHI_XE2_UP}
    {$DEFINE DELPHI_XE3_UP}
    {$DEFINE DELPHI_XE4_UP}
    {$DEFINE DELPHI_XE5_UP}
    {$DEFINE DELPHI_XE5}
    {$DEFINE DELPHI_XE6_UP}
    {$DEFINE DELPHI_XE6}
    {$DEFINE DELPHI_XE7_UP}
    {$DEFINE DELPHI_XE7}
    {$DEFINE DELPHI_XE8_UP}
    {$DEFINE DELPHI_XE8}
  {$ENDIF}

  {$IFDEF NEXTGEN}
    {$DEFINE DELPHI_NEXTGEN}
    {$DEFINE STANDARD_STRINGS_ONLY}
    {$ZEROBASEDSTRINGS OFF}
  {$ENDIF}

  {$IFDEF DELPHI_LEGACY}
    {$DEFINE CPUX86}
  {$ENDIF}

  {$IFDEF DELPHI_XE2_UP}
    {$IFNDEF PXL_SCIENTIFIC_MODE}
      {$EXCESSPRECISION OFF}
    {$ENDIF}
  {$ENDIF}

{$ENDIF}
