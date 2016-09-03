program km_unpack;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Types,
  Classes,
  Windows,
  Generics.Collections,
  km_archive in 'km_archive.pas';

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

var
  // операнд для ксора
  xor_base: Byte;
  // 4 байтовый операнд для ксора числовых значений в таблице
  xor_base_4b: Cardinal;
  // имя папки для запаковки и название архива, куда будет паковать
  folder_name, file_name: string;
  // список файлов для запаковки
  file_list: TStringDynArray;
  // запись в таблице
  chunk: TTableChunk;
  // файл-список, файл-архив и распаковываемый файл
  lst_file, arc_file, out_file: TFileStream;
  //lst_file: TXorStream;
  // некосренное содержимое файла-списка
  uncrypt_lst : TMemoryStream;
  // счётчик для цикла
  i: Integer;
  // оригинальный порядок файлов в таблице
  table_file_list: TStringList;
  // кол-во файлов в архиве
  file_count: Cardinal;
  new_file_name, new_file_ext: string;
begin
  try
    { TODO -oUser -cConsole Main : Insert code here }
    Writeln('Koihime Musou unpacker v 1.00');
    Writeln('Author: HeMet');
    Writeln('Jiyuu-VN: http://jiyuu-vn.ru');
    // параметров должно быть как минимум два
    if ParamCount < 2 then
    begin
      Writeln('Parameters count must be at least 2!');
      Exit;
    end;

    // первым идет название папки для запаковки
    folder_name := ParamStr(1);
    // вторым имя архива, куда будем паковать
    file_name := ParamStr(2);

    if FileExists(file_name) then
    begin
      // это файл-список
      lst_file := TFileStream.Create(ChangeFileExt(file_name, '.lst'), fmOpenRead);
      // а это файл-архив
      arc_file := TFileStream.Create(file_name, fmOpenRead);
    end
    else begin
      WriteLn('File doesn''t exist!');
      Exit;
    end;

    if not DirectoryExists(folder_name) then
    begin
      WriteLn('Folder doesn''t exist!');
      Exit;
    end;

    // по описанию формата файл заксорен по единице
    xor_base := 1;
    FillMemory(@xor_base_4b, 4, xor_base);

    Write('Uncrypting... ');
    // тут мы будем хранить расшифрованный файл-список
    uncrypt_lst := TMemoryStream.Create;
    XorStream(lst_file, uncrypt_lst, xor_base);
    uncrypt_lst.Position := 0;
    Writeln('Done.');

    uncrypt_lst.Read(file_count, SizeOf(file_count));
    Writeln('File count: ', file_count);

    // тут будем хранить имена файлов, что были в списке
    table_file_list := TStringList.Create;

    for i := 0 to file_count - 1 do
    begin
      chunk := ExtractChunk(uncrypt_lst);

      // с файле тип хранится нексоренным, но становится таким при ксоре
      // всего файла-списка, поэтому приводим его к начальному виду
      new_file_ext := FileTypeToExt(chunk.Extension xor xor_base_4b);

      table_file_list.Add(AnsiString(chunk.FileName) + new_file_ext);

      Write('Extract ', table_file_list[i], '... ');

      new_file_name := folder_name + '\' + table_file_list[i];

      out_file := TFileStream.Create(new_file_name, fmCreate);

      arc_file.Position := chunk.Offset;
      out_file.CopyFrom(arc_file, chunk.Size);

      FreeAndNil(out_file);

      Writeln('Done.');
    end;

    Write('Saving file list to ', ChangeFileExt(file_name, '.filelist'), '... ');
    table_file_list.SaveToFile(ChangeFileExt(file_name, '.filelist'));
    Writeln('Done.');

    FreeAndNil(uncrypt_lst);
    FreeAndNil(lst_file);
    FreeAndNil(arc_file);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
