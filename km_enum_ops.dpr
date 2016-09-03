program km_enum_ops;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Types,
  Classes,
  Windows,
  IOUtils,
  km_archive in 'km_archive.pas';

type
  TDBSInfo = record
    selector: Cardinal;
    operand_0: Cardinal;
    operand_1: Cardinal;
  end;

  TBytes15 = array[0..$f] of Byte;

  TOpVector = array[0..$ff] of Cardinal;

function ExtractChunk(Stream: TStream): TTableChunk;

  procedure ReplaceChar(var Buffer: array of AnsiChar; Src, Dest: AnsiChar);
  var
    i: Cardinal;
  begin
    for i := 0 to Length(Buffer) do
      if Buffer[i] = Src then
        Buffer[i] := Dest;
  end;

var
  chunk: TTableChunk;
begin
  Stream.Read(chunk, SizeOf(chunk));
  ReplaceChar(chunk.FileName, #1, #0);
  Result := chunk;
end;

function ExtractLabel(Stream: TStream): String;
var
  str_len: Cardinal;
  ansi_str: AnsiString;
begin
  Stream.Read(str_len, SizeOf(str_len));
  SetLength(ansi_str, str_len - 1);
  Stream.Read(ansi_str[1], Length(ansi_str));
  Stream.Seek(1, soFromCurrent);
  Result := ansi_str;
end;

function ExtractASCIIString(Stream: TStream; Index: Int64): TBytes15;
var
  buf: TBytes15;
begin
  Stream.Position := Index;
  Stream.Read(buf, 16);
  Result := buf;
end;

function InSS(Offset, ScrSize: Cardinal): Boolean;
begin
  Result := Offset < ScrSize;
end;

function ExtractCommand(Stream: TStream): TDBSInfo;
var
  buf: TDBSInfo;
begin
  Stream.Read(buf, SizeOf(buf));
  Result := buf;
end;

procedure EnumOperators(Stream: TStream; var Operators: TOpVector);
 var
  command_count: Cardinal;
  i: Integer;
  dbs: TDBSInfo;
begin
  command_count := 0;

  Stream.Position := 0;
  Stream.Read(command_count, SizeOf(command_count));
  Stream.Position := 8;

  for i := 0 to command_count - 1 do
  begin
    Stream.Read(dbs, SizeOf(dbs));
    Operators[dbs.selector] := 1;
  end;
end;

procedure ProcessFile(FileName: string; var Operators: TOpVector);
var
  script: TFileStream;
  uncrypted: TMemoryStream;
  crypted: Boolean;

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

  EnumOperators(uncrypted, Operators);

  FreeAndNil(uncrypted);
end;

procedure ProcessFiles(Files: TStringDynArray; var Operators: TOpVector);
var
  index: Integer;
begin
  for index := 0 to High(Files) do
  begin
    Writeln('Processing ', Files[index], ' ...');
    ProcessFile(Files[index], Operators);
  end;
end;

var
  folder_name: string;
  // список файлов для запаковки
  file_list: TStringDynArray;
  // счётчик для цикла
  i: Integer;
  record_list: TStringList;
  ops: TOpVector;
begin
  try
    { TODO -oUser -cConsole Main : Insert code here }
     if ParamCount < 1 then
    begin
      Writeln('Parameters count must be at least 1!');
      Exit;
    end;

    folder_name := ParamStr(1);

    if not TDirectory.Exists(folder_name) then
    begin
      WriteLn('Folder doesn''t exist!');
      Exit;
    end;

    file_list := TDirectory.GetFiles(folder_name, '*.snx', TSearchOption.soAllDirectories);

    ProcessFiles(file_list, ops);

    record_list := TStringList.Create;

    for i := 0 to $ff do
      if ops[i] = 1 then
        record_list.Add(IntToHex(i, 8));

    record_list.SaveToFile(folder_name + 'operator_enum.ops');

    FreeAndNil(record_list);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.

