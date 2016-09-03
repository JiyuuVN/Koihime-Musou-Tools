program km_find_dbs_ops;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Types,
  Classes,
  Windows,
  Generics.Collections,
  IOUtils,
  km_archive in 'km_archive.pas';

type
  TDBSInfo = record
    selector: Cardinal;
    operand_0: Cardinal;
    operand_1: Cardinal;
  end;

  TAddrList = TList<Cardinal>;
  TOpList = TList<TDBSInfo>;

function ExtractDataAddr(Stream: TStream): TAddrList;
var
  addr_list: TAddrList;
  chunk_size, chunk_addr: Cardinal;
  dbs_addr: Cardinal;
begin
  Stream.Position := 0;
  Stream.Read(dbs_addr, SizeOf(dbs_addr));

  dbs_addr := dbs_addr * 12 + 8;

  Stream.Position := dbs_addr;
  chunk_addr := 0;

  addr_list := TAddrList.Create;
  while Stream.Position < Stream.Size do
  begin
    addr_list.Add(chunk_addr);

    Stream.Read(chunk_size, SizeOf(chunk_size));
    Stream.Seek(chunk_size, soFromCurrent);

    chunk_addr := chunk_addr + chunk_size + 4;
  end;

  Result := addr_list;
end;

procedure FindDBSOps(Stream: TStream; DBSAddr: TAddrList; Operators, StrictFop,
    StrictSop: TOpList);
var
  command_count: Cardinal;
  op: TDBSInfo;
  i: Integer;
  fop_fuss_op, sop_fuss_op: array[0..$FF] of Byte;
  fop_is_addr, sop_is_addr: Boolean;

  function IsDBSAddr(Addr: Cardinal): Boolean;
  begin
    Result := (Addr <> 0) and (DBSAddr.IndexOf(Addr) > -1);
  end;

begin
  FillMemory(@fop_fuss_op, $ff, 0);
  FillMemory(@sop_fuss_op, $ff, 0);

  Stream.Position := 0;
  Stream.Read(command_count, SizeOf(command_count));

  Stream.Position := 8;

  for i := 0 to command_count - 1 do
  begin
     Stream.Read(op, SizeOf(op));

     fop_is_addr := IsDBSAddr(op.operand_0);
     sop_is_addr := IsDBSAddr(op.operand_1);

     if fop_is_addr or sop_is_addr then
     begin
       Operators.Add(op);

       if not fop_is_addr then
         fop_fuss_op[op.selector] := 1
       else
         sop_fuss_op[op.selector] := 1
     end;
  end;

  for i := 0 to Operators.Count - 1 do
  begin
    if (fop_fuss_op[Operators[i].selector] <> 1) then
      if Operators[i].operand_0 <> 0 then
        StrictFop.Add(Operators[i]);

    if (sop_fuss_op[Operators[i].selector] <> 1) then
      if Operators[i].operand_1 <> 0 then
        StrictSop.Add(Operators[i]);
  end;
end;

procedure ProcessFile(FileName: string; Operators, StrictFop, StrictSop:
    TOpList);
var
  script: TFileStream;
  uncrypted: TMemoryStream;
  crypted: Boolean;
  addr_list: TAddrList;

  function Xored(Stream: TStream): Boolean;
  var
    op_count, data_size: Cardinal;
  begin
    Stream.Position := 0;
    Stream.Read(op_count, SizeOf(op_count));
    Stream.Read(data_size, SizeOf(data_size));

    Result := (8 + op_count * SizeOf(TDBSInfo) + data_size) <> Stream.Size;
  end;

begin
  script := TFileStream.Create(FileName, fmOpenRead);
  uncrypted := TMemoryStream.Create;

  crypted := Xored(script);

  script.Position := 0;

  if not crypted then
  begin
    uncrypted.CopyFrom(script, 0);
  end else begin
    XorStream(script, uncrypted, $02);
    Writeln('Crypted');
  end;

  FreeAndNil(script);

  addr_list := ExtractDataAddr(uncrypted);

  FindDBSOps(uncrypted, addr_list, Operators, StrictFop, StrictSop);

  FreeAndNil(uncrypted);
end;

procedure ProcessFiles(Files: TStringDynArray; Operators, StrictFop, StrictSop:
    TOpList);
var
  index: Integer;
  op, fop, sop: TOpList;
begin
  for index := 0 to High(Files) do
  begin
    Writeln('Processing ', Files[index], ' ...');
    op := TOpList.Create;
    fop := TOpList.Create;
    sop := TOpList.Create;

//    ProcessFile(Files[index], Operators, StrictFop, StrictSop);
    ProcessFile(Files[index], Op, Fop, Sop);

    Operators.AddRange(op);
    StrictFop.AddRange(fop);
    StrictSop.AddRange(sop);

    FreeAndNil(op);
    FreeAndNil(fop);
    FreeAndNil(sop);
  end;
end;

var
  record_name, file_or_folder_name, folder_name: string;
  // список файлов для запаковки
  file_list: TStringDynArray;
  // счётчик для цикла
  i: Integer;
  // оригинальный порядок файлов в таблице
  record_list: TStringList;
  dbs: TDBSInfo;
  op_list, strict_fop, strict_sop: TOpList;
  print_soft_op: Boolean;
begin
  try
    { TODO -oUser -cConsole Main : Insert code here }
    if ParamCount < 1 then
    begin
      Writeln('Parameters count must be at least 1!');
      Exit;
    end;

    file_or_folder_name := ParamStr(1);
    print_soft_op := StrToBool(ParamStr(2));

    op_list := TOpList.Create;
    strict_fop := TOpList.Create;
    strict_sop := TOpList.Create;

    if FileExists(file_or_folder_name) then
    begin
      ProcessFile(file_or_folder_name, op_list, strict_fop, strict_sop);
    end
    else begin
      WriteLn('File doesn''t exist!');
    end;

    if TDirectory.Exists(file_or_folder_name) then
    begin
      file_list := TDirectory.GetFiles(file_or_folder_name, '*.snx', TSearchOption.soAllDirectories);

      ProcessFiles(file_list, op_list, strict_fop, strict_sop);

      file_or_folder_name := file_or_folder_name + 'operators';
    end;

    record_list := TStringList.Create;

    if print_soft_op then
    begin
      record_list.Add('Soft ops');
      for dbs in op_list do
        record_list.Add(IntToHex(dbs.selector, 2) + #9 + IntToHex(dbs.operand_0, 8) + #9 + IntToHex(dbs.operand_1, 8));
    end;

    record_list.Add('Strict first op');
    for dbs in strict_fop do
      record_list.Add(IntToHex(dbs.selector, 2) + #9 + IntToHex(dbs.operand_0, 8) + #9 + IntToHex(dbs.operand_1, 8));

    record_list.Add('Strict second op');
    for dbs in strict_sop do
      record_list.Add(IntToHex(dbs.selector, 2) + #9 + IntToHex(dbs.operand_0, 8) + #9 + IntToHex(dbs.operand_1, 8));

    FreeAndNil(op_list);
    FreeAndNil(strict_fop);
    FreeAndNil(strict_sop);

    record_list.SaveToFile(ChangeFileExt(file_or_folder_name, '.opl'));
    FreeAndNil(record_list);

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.

