program km_snx_comp;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Types,
  Classes,
  Windows,
  Generics.Collections,
  IOUtils,
  NativeXml,
  km_archive in 'km_archive.pas',
  km_snx in 'km_snx.pas';

procedure XMLToData(Xml: TNativeXml; Data: TStringList);
var
  i: Integer;
begin
  for i := 0 to Xml.Root.NodeCount - 1 do
  begin
    Data.Add(Xml.Root.Nodes[i].FindNode('ToTranslate').ValueAsString);
  end;
end;

function StringToDataChunk(Str: string): TDataChunk;
var
  ansi_str: AnsiString;
  buf: TStringBuilder;
  chunk: TDataChunk;
begin
  buf := TStringBuilder.Create(Str);

  buf := buf.Replace(CHR_PRINT_TEXT_REPLACE, CHR_PRINT_TEXT);
  buf := buf.Replace(STR_WAIT_FOR_INPUT_REPLACE, STR_WAIT_FOR_INPUT);
  buf := buf.Replace(CHR_TWO_REPLACE, CHR_TWO);
  buf := buf.Replace(CHR_THREE_REPLACE, CHR_THREE);

  ansi_str := buf.ToString + #0;

  SetLength(chunk, Length(ansi_str));

  CopyMemory(@chunk[0], @ansi_str[1], Length(ansi_str));

  Result := chunk;

  FreeAndNil(buf);
end;

procedure MakeDBS(Data: TStringList; DBS: TStream; DataAddr: TDataAddr);
var
  i: Integer;
  chunk: TDataChunk;
  chunk_addr, chunk_size: Cardinal;
begin
  DBS.Size := 0;

  for i := 0 to Data.Count - 1 do
  begin
    DataAddr.Add(DBS.Position);

    chunk := StringToDataChunk(Data[i]);
    chunk_size := Length(chunk);

    DBS.Write(chunk_size, SizeOf(chunk_size));
    DBS.Write(chunk[0], chunk_size);
  end;
end;

procedure RebuildCS(CS, Dump: TStream; DataAddr: TDataAddr);
var
  command_count: Cardinal;
  i: Integer;
  command: TSNXCommand;
begin
  command_count := Dump.Size div SizeOf(TSNXCommand);

  for i := 0 to command_count - 1 do
  begin
    Dump.Read(command, SizeOf(TSNXCommand));

    if ThisDBSReadCommand(command) then
      command.SValue := DataAddr[command.SValue];

    CS.Write(command, SizeOf(TSNXCommand))
  end;
end;

procedure CompileSNX(SNX, CS, DBS: TStream);
var
  command_count: Cardinal;
  dbs_size: Cardinal;
begin
  SNX.Size := 0;

  command_count := CS.Size div SizeOf(TSNXCommand);
  dbs_size := DBS.Size;

  SNX.Write(command_count, SizeOf(command_count));
  SNX.Write(dbs_size, SizeOf(dbs_size));

  CS.Position := 0;
  SNX.CopyFrom(CS, 0);
  DBS.Position := 0;
  SNX.CopyFrom(DBS, 0);
end;

procedure LoadDecompDBS(FileName: string; DBS: TStream; DataAddr: TDataAddr);
var
  xml: TNativeXml;
  data: TStringList;
begin
  data := TStringList.Create;

  xml := TNativeXml.Create;
  xml.LoadFromFile(ChangeFileExt(FileName, '.xml'));
  XMLToData(xml, data);
  FreeAndNil(xml);

  MakeDBS(data, DBS, DataAddr);

  FreeAndNil(data);
end;

procedure LoadCodeDump(FileName: string; DataAddr: TDataAddr; CS: TStream);
var
  snx: TFileStream;
begin
  snx := TFileStream.Create(ChangeFileExt(FileName, '.opl'), fmOpenRead);

  CS.Size := 0;
  RebuildCS(CS, snx, DataAddr);

  FreeAndNil(snx);
end;

procedure LoadDecompSNX(FileName: string; CS, DBS: TStream);
var
  data_addr: TDataAddr;
begin
  data_addr := TDataAddr.Create;

  LoadDecompDBS(FileName, DBS, data_addr);

  LoadCodeDump(FileName, data_addr, CS);
end;

procedure SaveSNX(FileName: string; CS, DBS: TStream; Crypt: Boolean = False);
var
  snx_file: TFileStream;
  snx_body: TMemoryStream;
  command_count, dbs_size: Cardinal;
begin
  snx_body := TMemoryStream.Create;

  command_count := CS.Size div SizeOf(TSNXCommand);
  dbs_size := DBS.Size;

  snx_body.Write(command_count, SizeOf(command_count));
  snx_body.Write(dbs_size, SizeOf(dbs_size));

  CS.Position := 0;
  snx_body.CopyFrom(CS, 0);
  DBS.Position := 0;
  snx_body.CopyFrom(DBS, 0);

  snx_file := TFileStream.Create(ChangeFileExt(FileName, '.snx'), fmCreate);

  snx_body.Position := 0;
  if Crypt then
    XorStream(snx_body, snx_file, SNX_CRYPT_VALUE)
  else
    snx_file.CopyFrom(snx_body, 0);

  FreeAndNil(snx_file);
end;

var
  file_name: string;
  crypt: Boolean;
  CS, DBS: TMemoryStream;
begin
  try
    Writeln('Koihime Musou SNX recompiler v 1.02');
    Writeln('Author: HeMet');
    Writeln('Jiyuu-VN: http://jiyuu-vn.ru');
    { TODO -oUser -cConsole Main : Insert code here }
    file_name := ParamStr(1);
    crypt := StrToBool(ParamStr(2));

    if FileExists(ChangeFileExt(file_name, '.opl'))
      and FileExists(ChangeFileExt(file_name, '.xml')) then
    begin
      CS := TMemoryStream.Create;
      DBS := TMemoryStream.Create;

      LoadDecompSNX(file_name, CS, DBS);

      SaveSNX(file_name, CS, DBS, crypt);

      FreeAndNil(CS);
      FreeAndNil(DBS);
    end else begin
      WriteLn('File doesn''t exist!');
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
