program km_snx_decomp;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Classes,
  Generics.Collections,
  NativeXml,
  km_archive in 'km_archive.pas',
  km_snx in 'km_snx.pas';

// DBS - Data Base Section
function ExtractDataAddr(DBS: TStream): TDataAddr;
var
  addr_list: TDataAddr;
  chunk_size, chunk_addr: Cardinal;
begin
  DBS.Position := 0;

  addr_list := TDataAddr.Create;
  while DBS.Position < DBS.Size do
  begin
    chunk_addr := DBS.Position;

    addr_list.Add(chunk_addr);

    DBS.Read(chunk_size, SizeOf(chunk_size));
    DBS.Seek(chunk_size, soFromCurrent);
  end;

  Result := addr_list;
end;

procedure DumpSC(SC, Dump: TStream; DataAddr: TDataAddr);
var
  command_count, i: Cardinal;
  command: TSNXCommand;
begin
  command_count := SC.Size div SizeOf(TSNXCommand);

  SC.Position := 0;

  for i := 0 to command_count - 1 do
  begin
    SC.Read(command, SizeOf(command));

    if ThisDBSReadCommand(command) then
      command.SValue := DataAddr.IndexOf(command.SValue);

    Dump.Write(command, SizeOf(command));
  end;

end;

function DataChunkToString(Data: TDataChunk): string;
var
  ansi_str: AnsiString;
  buf: TStringBuilder;
begin
  ansi_str := AnsiString(Data);

  buf := TStringBuilder.Create(PAnsiChar(@Data[0]));

  buf := buf.Replace(CHR_PRINT_TEXT, CHR_PRINT_TEXT_REPLACE);
  buf := buf.Replace(STR_WAIT_FOR_INPUT, STR_WAIT_FOR_INPUT_REPLACE);
  buf := buf.Replace(CHR_TWO, CHR_TWO_REPLACE);
  buf := buf.Replace(CHR_THREE, CHR_THREE_REPLACE);

  Result := buf.ToString;
  FreeAndNil(buf);
end;

procedure ParseDBS(DBS: TStream; Data: TStringList);
var
  chunk_size: Cardinal;
  chunk: TDataChunk;
  data_str: string;

begin
  DBS.Position := 0;

  while DBS.Position < DBS.Size do
  begin
    DBS.Read(chunk_size, SizeOf(chunk_size));

    // завершающий нуль нам не нужен
    SetLength(chunk, chunk_size);
    DBS.Read(chunk[0], Length(chunk));

    data_str := DataChunkToString(chunk);

    Data.Add(data_str);
  end;
end;

procedure GetSnxSections(SNX, CS, DBS: TStream);
var
  command_count: Cardinal;
  dbs_size: Cardinal;
begin
   SNX.Read(command_count, SizeOf(command_count));
   SNX.Read(dbs_size, SizeOf(dbs_size));

   CS.Size := 0;
   CS.CopyFrom(SNX, command_count * SizeOf(TSNXCommand));

   DBS.Size := 0;
   DBS.CopyFrom(SNX, dbs_size);
end;

procedure DecompileSNX(SNX: TStream; CodeDump: TStream; Data: TStringList);
var
  code, dbs: TMemoryStream;
  data_addr: TDataAddr;
begin
  code := TMemoryStream.Create;
  dbs := TMemoryStream.Create;

  GetSnxSections(SNX, code, dbs);

  data_addr := ExtractDataAddr(dbs);
  DumpSC(code, CodeDump, data_addr);

  ParseDBS(dbs, Data);

  FreeAndNil(data_addr);
  FreeAndNil(code);
  FreeAndNil(dbs);
end;

procedure DataToXML(XML: TNativeXml; Data: TStringList; DublicateStrings:
    Boolean);
var
  i: Integer;
begin
  for i := 0 to Data.Count - 1 do
  begin
    XML.Root.Name := 'DBS';
    with XML.Root.NodeNew('String') do
    begin
      AttributeAdd('index', IntToStr(i));
      if DublicateStrings then NodeNew('Origin').ValueAsString := Data[i];
      NodeNew('ToTranslate').ValueAsString := Data[i];
    end;
  end;
end;

procedure SaveDecompSNX(FileName: string; CodeDump: TStream; Data: TStringList;
    DublicateStrings: Boolean);
var
  opl: TFileStream;
  xml: TNativeXml;
begin
  opl := TFileStream.Create(ChangeFileExt(FileName, '.opl'), fmCreate);
  opl.CopyFrom(CodeDump, 0);
  FreeAndNil(opl);

  xml := TNativeXml.Create;
  xml.XmlFormat := xfReadable;
  DataToXML(xml, Data, DublicateStrings);
  xml.SaveToFile(ChangeFileExt(FileName, '.xml'));
  FreeAndNil(xml);
end;

procedure LoadSNX(FileName: string; Stream: TStream);
var
  snx: TFileStream;
begin
  snx := TFileStream.Create(FileName, fmOpenRead);

  Stream.Size := 0;

  if Xored(snx) then
  begin
    snx.Position := 0;
    XorStream(snx, Stream, $02);
  end else begin
    snx.Position := 0;
    Stream.CopyFrom(snx, 0);
  end;

  FreeAndNil(snx);
end;

var
  file_name: string;
  snx: TMemoryStream;
  code: TMemoryStream;
  data: TStringList;
  add_origin: Boolean;

begin
  try
    Writeln('Koihime Musou SNX decompiler v 1.02');
    Writeln('Author: HeMet');
    Writeln('Jiyuu-VN: http://jiyuu-vn.ru');
    { TODO -oUser -cConsole Main : Insert code here }
    file_name := ParamStr(1);
    add_origin := StrToBool(ParamStr(2));

    if FileExists(file_name) then
    begin
      snx := TMemoryStream.Create;
      LoadSNX(file_name, snx);
    end else begin
      WriteLn('File doesn''t exist!');
      Exit;
    end;

    code := TMemoryStream.Create;
    data := TStringList.Create;

    snx.Position := 0;
    DecompileSNX(snx, code, data);

    SaveDecompSNX(file_name, code, data, add_origin);

    FreeAndNil(data);
    FreeAndNil(code);
    FreeAndNil(snx);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
