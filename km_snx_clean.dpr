program km_snx_clean;

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

  TBytes15 = array[0..$f] of Byte;

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

var
  // операнд для ксора
  xor_base: Byte;
  // 4 байтовый операнд для ксора числовых значений в таблице
  xor_base_4b: Cardinal;
  // имя папки для запаковки и название архива, куда будет паковать
  record_name, file_name, folder_name: string;
  // список файлов для запаковки
  file_list: TStringDynArray;
  // запись в таблице
  chunk: TTableChunk;
  // файл-список, файл-архив и распаковываемый файл
  snx_file, arc_file, out_file: TFileStream;
  // некосренное содержимое файла-списка
  uncrypt_lst : TMemoryStream;
  // счётчик для цикла
  i: Integer;
  // оригинальный порядок файлов в таблице
  record_list: TStringList;
  // кол-во файлов в архиве
  offset, scr_size: Cardinal;
  index: Int64;
  dbs: TDBSInfo;
  buf: TBytes15;
  ascii: AnsiString;
  op: Cardinal;
begin
  try
    { TODO -oUser -cConsole Main : Insert code here }
    // параметров должно быть как минимум два
    if ParamCount < 1 then
    begin
      Writeln('Parameters count must be at least 1!');
      Exit;
    end;

    // вторым имя архива, куда будем паковать
    file_name := ParamStr(1);
    folder_name := ParamStr(2);
    op := StrToInt(ParamStr(3));

    if FileExists(file_name) then
    begin
      snx_file := TFileStream.Create(file_name, fmOpenRead);
    end
    else begin
      WriteLn('File doesn''t exist!');
      Exit;
    end;

    xor_base := 2;

    //Write('Uncrypting... ');
    uncrypt_lst := TMemoryStream.Create;
    //XorStream(snx_file, uncrypt_lst, xor_base);
    //uncrypt_lst.Position := 0;
    //Writeln('Done.');

    snx_file.Read(offset, SizeOf(offset));
    snx_file.Read(scr_size, SizeOf(scr_size));

    record_list := TStringList.Create;

    record_list.Add('DBS Section');
    for i := 0 to offset - 1 do
    begin
      snx_file.Read(dbs, SizeOf(dbs));
      record_list.Add(IntToStr(dbs.selector) + #9 + IntToStr(dbs.operand_0) + #9 + IntToStr(dbs.operand_1));
      if (dbs.selector = op) then
      begin

        index := snx_file.Position;
        buf := ExtractASCIIString(snx_file, 12 * offset + 8 + dbs.operand_0);
        snx_file.Position := index;

        uncrypt_lst.Write(dbs.selector, 4);
        uncrypt_lst.Write(dbs.operand_0, 4);
        uncrypt_lst.Write(dbs.operand_1, 4);
        index := 0;
        uncrypt_lst.Write(index, 4);

        uncrypt_lst.Write(buf, 16);
      end;

      {if (dbs.selector <> $11) and InSS(dbs.operand_1, scr_size) then
      begin

        index := snx_file.Position;
        buf := ExtractASCIIString(snx_file, 12 * offset + 8 + dbs.operand_1);
        snx_file.Position := index;

        uncrypt_lst.Write(dbs.selector, 4);
        uncrypt_lst.Write(dbs.operand_0, 4);
        uncrypt_lst.Write(dbs.operand_1, 4);
        index := 0;
        uncrypt_lst.Write(index, 4);

        uncrypt_lst.Write(buf, 16);
      end;}
    end;
    record_list.Add('');

    record_list.Add('Script block');
    while snx_file.Position < snx_file.Size do
    begin
      record_name := ExtractLabel(snx_file);
      Writeln(record_name);
      record_list.Add(record_name);
    end;
    record_list.SaveToFile(ChangeFileExt(file_name, '.rec'));
    uncrypt_lst.SaveToFile(ChangeFileExt(file_name, '.mem'));

    FreeAndNil(uncrypt_lst);
    FreeAndNil(snx_file);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.

