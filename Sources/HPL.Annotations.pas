unit HPL.Annotations;
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
  TAnnotationAccess = (None, Read, Write, ReadWrite);

  PAnnotationInfo = ^TAnnotationInfo;
  TAnnotationInfo = record
    Name: StdString;
    Access: TAnnotationAccess;
    Optional: Boolean;
    Multiple: Boolean;
  end;

  TAnnotations = class
  private
    FList: array of TAnnotationInfo;

    procedure InitList;
    procedure SwapList(const Index1, Index2: Integer);
    function CompareList(const Index1, Index2: Integer): Integer; inline;
    function SplitList(const Start, Stop: Integer): Integer;
    procedure SortList(const Start, Stop: Integer);
    procedure UpdateList;
  protected
    function IndexOf(const Name: StdString): Integer;
  public
    constructor Create;

    function GetAnnotation(const Name: StdString; out Info: TAnnotationInfo): Boolean;
    function Exists(const Name: StdString): Boolean;
  end;

implementation

uses
  SysUtils;

{$REGION 'Global Constants'}

const
  KnownAnnotations: array[0..136] of TAnnotationInfo = (
    (Name: '_COM_Outptr_'; Access: TAnnotationAccess.Write; Optional: False),
    (Name: '_COM_Outptr_opt_'; Access: TAnnotationAccess.Write; Optional: True),
    (Name: '_COM_Outptr_opt_result_maybenull_'; Access: TAnnotationAccess.Write),
    (Name: '_COM_Outptr_result_maybenull_'; Access: TAnnotationAccess.Write; Optional: False),
    (Name: '_Deref_in_range_'; Access: TAnnotationAccess.Read; Optional: False),
    (Name: '_Deref_inout_range_'; Access: TAnnotationAccess.ReadWrite; Optional: False),
    (Name: '_Deref_out_range_'; Access: TAnnotationAccess.Write; Optional: False),
    (Name: '_Field_range_'; Access: TAnnotationAccess.None; Optional: False),
    (Name: '_Field_size_'; Access: TAnnotationAccess.None; Optional: False),
    (Name: '_Field_size_opt_'; Access: TAnnotationAccess.None; Optional: True),
    (Name: '_In_'; Access: TAnnotationAccess.Read; Optional: False),
    (Name: '_In_opt_'; Access: TAnnotationAccess.Read; Optional: True),
    (Name: '_In_opt_z_'; Access: TAnnotationAccess.Read; Optional: True),
    (Name: '_In_range_'; Access: TAnnotationAccess.Read; Optional: False),
    (Name: '_In_reads_'; Access: TAnnotationAccess.Read; Optional: False),
    (Name: '_In_reads_bytes_'; Access: TAnnotationAccess.Read; Optional: False),
    (Name: '_In_reads_bytes_opt_'; Access: TAnnotationAccess.Read; Optional: True),
    (Name: '_In_reads_opt_'; Access: TAnnotationAccess.Read; Optional: True),
    (Name: '_In_reads_opt_z_'; Access: TAnnotationAccess.Read; Optional: True),
    (Name: '_In_reads_or_z_'; Access: TAnnotationAccess.Read; Optional: False),
    (Name: '_In_reads_to_ptr_'; Access: TAnnotationAccess.Read; Optional: False),
    (Name: '_In_reads_to_ptr_opt_'; Access: TAnnotationAccess.Read; Optional: True),
    (Name: '_In_reads_to_ptr_opt_z_'; Access: TAnnotationAccess.Read; Optional: True),
    (Name: '_In_reads_to_ptr_z_'; Access: TAnnotationAccess.Read; Optional: False),
    (Name: '_In_reads_z_'; Access: TAnnotationAccess.Read; Optional: False),
    (Name: '_In_z_'; Access: TAnnotationAccess.Read; Optional: False),
    (Name: '_Inout_'; Access: TAnnotationAccess.ReadWrite; Optional: False),
    (Name: '_Inout_opt_'; Access: TAnnotationAccess.ReadWrite; Optional: True),
    (Name: '_Inout_opt_bytecount_'; Access: TAnnotationAccess.ReadWrite; Optional: True),
    (Name: '_Inout_opt_z_'; Access: TAnnotationAccess.ReadWrite; Optional: True),
    (Name: '_Inout_updates_'; Access: TAnnotationAccess.ReadWrite; Optional: False),
    (Name: '_Inout_updates_all_'; Access: TAnnotationAccess.ReadWrite; Optional: False),
    (Name: '_Inout_updates_all_opt_'; Access: TAnnotationAccess.ReadWrite; Optional: True),
    (Name: '_Inout_updates_bytes_'; Access: TAnnotationAccess.ReadWrite; Optional: False),
    (Name: '_Inout_updates_bytes_all_'; Access: TAnnotationAccess.ReadWrite; Optional: False),
    (Name: '_Inout_updates_bytes_all_opt_'; Access: TAnnotationAccess.ReadWrite; Optional: True),
    (Name: '_Inout_updates_bytes_opt_'; Access: TAnnotationAccess.ReadWrite; Optional: True),
    (Name: '_Inout_updates_bytes_to_'; Access: TAnnotationAccess.ReadWrite; Optional: False),
    (Name: '_Inout_updates_bytes_to_'; Access: TAnnotationAccess.ReadWrite; Optional: False),
    (Name: '_Inout_updates_bytes_to_opt_'; Access: TAnnotationAccess.ReadWrite; Optional: True),
    (Name: '_Inout_updates_opt_'; Access: TAnnotationAccess.ReadWrite; Optional: True),
    (Name: '_Inout_updates_opt_z_'; Access: TAnnotationAccess.ReadWrite; Optional: True),
    (Name: '_Inout_updates_to_'; Access: TAnnotationAccess.ReadWrite; Optional: False),
    (Name: '_Inout_updates_to_'; Access: TAnnotationAccess.ReadWrite; Optional: False),
    (Name: '_Inout_updates_to_opt_'; Access: TAnnotationAccess.ReadWrite; Optional: True),
    (Name: '_Inout_updates_z_'; Access: TAnnotationAccess.ReadWrite; Optional: False),
    (Name: '_Inout_z_'; Access: TAnnotationAccess.ReadWrite; Optional: False),
    (Name: '_Outptr_opt_result_maybenull_z_'; Access: TAnnotationAccess.Write; Optional: True),
    (Name: '_Out_'; Access: TAnnotationAccess.Write; Optional: False),
    (Name: '_Out_opt_'; Access: TAnnotationAccess.Write; Optional: True),
    (Name: '_Out_range_'; Access: TAnnotationAccess.Write; Optional: False),
    (Name: '_Out_writes_'; Access: TAnnotationAccess.Write; Optional: False),
    (Name: '_Out_writes_all_'; Access: TAnnotationAccess.Write; Optional: False),
    (Name: '_Out_writes_all_opt_'; Access: TAnnotationAccess.Write; Optional: True),
    (Name: '_Out_writes_bytes_'; Access: TAnnotationAccess.Write; Optional: False),
    (Name: '_Out_writes_bytes_all_'; Access: TAnnotationAccess.Write; Optional: False),
    (Name: '_Out_writes_bytes_all_opt_'; Access: TAnnotationAccess.Write; Optional: True),
    (Name: '_Out_writes_bytes_opt_'; Access: TAnnotationAccess.Write; Optional: True),
    (Name: '_Out_writes_bytes_to_'; Access: TAnnotationAccess.Write; Optional: False),
    (Name: '_Out_writes_bytes_to_'; Access: TAnnotationAccess.Write; Optional: False),
    (Name: '_Out_writes_bytes_to_opt_'; Access: TAnnotationAccess.Write; Optional: True),
    (Name: '_Out_writes_opt_'; Access: TAnnotationAccess.Write; Optional: True),
    (Name: '_Out_writes_opt_z_'; Access: TAnnotationAccess.Write; Optional: True),
    (Name: '_Out_writes_to_'; Access: TAnnotationAccess.Write; Optional: False),
    (Name: '_Out_writes_to_'; Access: TAnnotationAccess.Write; Optional: False),
    (Name: '_Out_writes_to_opt_'; Access: TAnnotationAccess.Write; Optional: True),
    (Name: '_Out_writes_to_ptr_'; Access: TAnnotationAccess.Write; Optional: False),
    (Name: '_Out_writes_to_ptr_opt_'; Access: TAnnotationAccess.Write; Optional: True),
    (Name: '_Out_writes_to_ptr_opt_z_'; Access: TAnnotationAccess.Write; Optional: True),
    (Name: '_Out_writes_to_ptr_z_'; Access: TAnnotationAccess.Write; Optional: False),
    (Name: '_Out_writes_z_'; Access: TAnnotationAccess.Write; Optional: False),
    (Name: '_Outptr_'; Access: TAnnotationAccess.Write; Optional: False),
    (Name: '_Outptr_opt_'; Access: TAnnotationAccess.Write; Optional: True),
    (Name: '_Outptr_opt_result_buffer_'; Access: TAnnotationAccess.Write; Optional: True),
    (Name: '_Outptr_opt_result_buffer_to_'; Access: TAnnotationAccess.Write; Optional: True),
    (Name: '_Outptr_opt_result_bytebuffer_'; Access: TAnnotationAccess.Write; Optional: True),
    (Name: '_Outptr_opt_result_bytebuffer_to_'; Access: TAnnotationAccess.Write; Optional: True),
    (Name: '_Outptr_opt_result_maybenull_'; Access: TAnnotationAccess.Write; Optional: True),
    (Name: '_Outptr_opt_result_nullonfailure_'; Access: TAnnotationAccess.Write; Optional: True),
    (Name: '_Outptr_opt_result_z_'; Access: TAnnotationAccess.Write; Optional: True),
    (Name: '_Outptr_result_buffer_'; Access: TAnnotationAccess.Write; Optional: False),
    (Name: '_Outptr_result_buffer_to_'; Access: TAnnotationAccess.Write; Optional: False),
    (Name: '_Outptr_result_bytebuffer_'; Access: TAnnotationAccess.Write; Optional: False),
    (Name: '_Outptr_result_bytebuffer_to_'; Access: TAnnotationAccess.Write; Optional: False),
    (Name: '_Outptr_result_maybenull_'; Access: TAnnotationAccess.Write; Optional: False),
    (Name: '_Outptr_result_maybenull_z_'; Access: TAnnotationAccess.Write; Optional: False),
    (Name: '_Outptr_result_nullonfailure_'; Access: TAnnotationAccess.Write; Optional: False),
    (Name: '_Outptr_result_z_'; Access: TAnnotationAccess.Write; Optional: False),
    (Name: '_Outref_'; Access: TAnnotationAccess.Write; Optional: False),
    (Name: '_Outref_result_buffer_'; Access: TAnnotationAccess.Write; Optional: False),
    (Name: '_Outref_result_buffer_all_'; Access: TAnnotationAccess.Write; Optional: False),
    (Name: '_Outref_result_buffer_all_maybenull_'; Access: TAnnotationAccess.Write; Optional: False),
    (Name: '_Outref_result_buffer_maybenull_'; Access: TAnnotationAccess.Write; Optional: False),
    (Name: '_Outref_result_buffer_to_'; Access: TAnnotationAccess.Write; Optional: False),
    (Name: '_Outref_result_buffer_to_maybenull_'; Access: TAnnotationAccess.Write; Optional: False),
    (Name: '_Outref_result_bytebuffer_'; Access: TAnnotationAccess.Write; Optional: False),
    (Name: '_Outref_result_bytebuffer_all_'; Access: TAnnotationAccess.Write; Optional: False),
    (Name: '_Outref_result_bytebuffer_all_maybenull_'; Access: TAnnotationAccess.Write; Optional: False),
    (Name: '_Outref_result_bytebuffer_maybenull_'; Access: TAnnotationAccess.Write; Optional: False),
    (Name: '_Outref_result_bytebuffer_to_'; Access: TAnnotationAccess.Write; Optional: False),
    (Name: '_Outref_result_bytebuffer_to_maybenull_'; Access: TAnnotationAccess.Write; Optional: False),
    (Name: '_Outref_result_maybenull_'; Access: TAnnotationAccess.Write; Optional: False),
    (Name: '_Outref_result_nullonfailure_'; Access: TAnnotationAccess.Write; Optional: False),
    (Name: '_Post_equal_to_'; Access: TAnnotationAccess.None; Optional: False),
    (Name: '_Pre_equal_to_'; Access: TAnnotationAccess.None; Optional: False),
    (Name: '_Result_nullonfailure_'; Access: TAnnotationAccess.Write; Optional: False),
    (Name: '_Result_zeroonfailure_'; Access: TAnnotationAccess.Write; Optional: False),
    (Name: '_Ret_maybenull_'; Access: TAnnotationAccess.Write; Optional: False),
    (Name: '_Ret_maybenull_z_'; Access: TAnnotationAccess.Write; Optional: False),
    (Name: '_Ret_notnull_'; Access: TAnnotationAccess.Write; Optional: False),
    (Name: '_Ret_null_'; Access: TAnnotationAccess.Write; Optional: False),
    (Name: '_Ret_range_'; Access: TAnnotationAccess.Write; Optional: False),
    (Name: '_Ret_writes_'; Access: TAnnotationAccess.Write; Optional: False),
    (Name: '_Ret_writes_bytes_'; Access: TAnnotationAccess.Write; Optional: False),
    (Name: '_Ret_writes_bytes_maybenull_'; Access: TAnnotationAccess.Write; Optional: False),
    (Name: '_Ret_writes_bytes_to_'; Access: TAnnotationAccess.Write; Optional: False),
    (Name: '_Ret_writes_bytes_to_maybenull_'; Access: TAnnotationAccess.Write; Optional: False),
    (Name: '_Ret_writes_maybenull_'; Access: TAnnotationAccess.Write; Optional: False),
    (Name: '_Ret_writes_maybenull_z_'; Access: TAnnotationAccess.Write; Optional: False),
    (Name: '_Ret_writes_to_'; Access: TAnnotationAccess.Write; Optional: False),
    (Name: '_Ret_writes_to_maybenull_'; Access: TAnnotationAccess.Write; Optional: False),
    (Name: '_Ret_writes_z_'; Access: TAnnotationAccess.Write; Optional: False),
    (Name: '_Ret_z_'; Access: TAnnotationAccess.Write; Optional: False),
    (Name: '_Struct_size_bytes_'; Access: TAnnotationAccess.None; Optional: False),
    (Name: '__RPC__deref_opt_inout_opt'; Access: TAnnotationAccess.ReadWrite; Optional: True),
    (Name: '__RPC__deref_out_ecount_full_opt'; Access: TAnnotationAccess.Write; Optional: True),
    (Name: '__RPC__deref_out_opt'; Access: TAnnotationAccess.Write; Optional: True),
    (Name: '__RPC__in'; Access: TAnnotationAccess.Read; Optional: False),
    (Name: '__RPC__in_ecount_full'; Access: TAnnotationAccess.Read; Optional: False),
    (Name: '__RPC__in_opt'; Access: TAnnotationAccess.Read; Optional: True),
    (Name: '__RPC__inout'; Access: TAnnotationAccess.ReadWrite; Optional: False),
    (Name: '__RPC__inout_ecount_full'; Access: TAnnotationAccess.ReadWrite; Optional: False),
    (Name: '__RPC__inout_ecount_full_opt'; Access: TAnnotationAccess.ReadWrite; Optional: True),
    (Name: '__RPC__inout_opt'; Access: TAnnotationAccess.ReadWrite; Optional: True),
    (Name: '__RPC__out'; Access: TAnnotationAccess.Write; Optional: False),
    (Name: '__RPC__out_ecount_full'; Access: TAnnotationAccess.Write; Optional: False),
    (Name: '__RPC_unique_pointer'; Access: TAnnotationAccess.None; Optional: False)
  );

{$ENDREGION}
{$REGION 'TAnnotations'}

constructor TAnnotations.Create;
begin
  inherited;

  UpdateList;
end;

procedure TAnnotations.InitList;
var
  I: Integer;
begin
  SetLength(FList, High(KnownAnnotations) + 1);

  for I := 0 to Length(FList) - 1 do
    FList[I] := KnownAnnotations[I];
end;

procedure TAnnotations.SwapList(const Index1, Index2: Integer);
var
  TempValue: TAnnotationInfo;
begin
  TempValue := FList[Index1];
  FList[Index1] := FList[Index2];
  FList[Index2] := TempValue;
end;

function TAnnotations.CompareList(const Index1, Index2: Integer): Integer;
begin
  Result := CompareStr(FList[Index1].Name, FList[Index2].Name);
end;

function TAnnotations.SplitList(const Start, Stop: Integer): Integer;
var
  Left, Right, Pivot: Integer;
begin
  Left := Start + 1;
  Right := Stop;
  Pivot := Start;

  while Left <= Right do
  begin
    while (Left <= Stop) and (CompareList(Left, Pivot) < 0) do
      Inc(Left);

    while (Right > Start) and (CompareList(Right, Pivot) >= 0) do
      Dec(Right);

    if Left < Right then
      SwapList(Left, Right);
  end;

  SwapList(Start, Right);
  Result := Right;
end;

procedure TAnnotations.SortList(const Start, Stop: Integer);
var
  SplitPt: Integer;
begin
  if Start < Stop then
  begin
    SplitPt := SplitList(Start, Stop);
    SortList(Start, SplitPt - 1);
    SortList(SplitPt + 1, Stop);
  end;
end;

procedure TAnnotations.UpdateList;
begin
  InitList;

  if Length(FList) > 1 then
    SortList(0, Length(FList) - 1);
end;

function TAnnotations.IndexOf(const Name: StdString): Integer;
var
  Left, Right, Pivot, Res: Integer;
begin
  Left := 0;
  Right := Length(FList) - 1;

  while Left <= Right do
  begin
    Pivot := (Left + Right) div 2;
    Res := CompareStr(FList[Pivot].Name, Name);

    if Res = 0 then
      Exit(Pivot);

    if Res > 0 then
      Right := Pivot - 1
    else
      Left := Pivot + 1;
  end;

  Result := -1;
end;

function TAnnotations.GetAnnotation(const Name: StdString; out Info: TAnnotationInfo): Boolean;
var
  Index: Integer;
begin
  Index := IndexOf(Name);

  Result := Index <> -1;

  if Result then
    Info := FList[Index];
end;

function TAnnotations.Exists(const Name: StdString): Boolean;
begin
  Result := IndexOf(Name) <> -1;
end;

{$ENDREGION}

end.
