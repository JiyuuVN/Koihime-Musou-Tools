program km_pack;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Classes,
  IOUtils,
  Types,
  Windows,
  Generics.Collections,
  Generics.Defaults,
  km_archive in 'km_archive.pas';

// извлекает из потоков добавляемого файла и файла-архива всю необходимую
// инфу для записи в таблице
function MakeChunk(Stream: TFileStream; ArcStream: TStream; XorValue: Byte =
    1): TTableChunk;
var
  chunk: TTableChunk;
  file_name: AnsiString;
  file_ext: AnsiString;
  // 4 байтовое число для ксора типа файла
  xor_base_4b: Cardinal;
begin
  file_name := ExtractFileName(Stream.FileName);
  file_ext := ExtractFileExt(Stream.FileName);

  // в таблице хранятся только имена без путей, поэтому отрезаем расширение
  file_name := Copy(file_name, 0, Length(file_name) - Length(file_ext));
  // убираем из расшерения точку
  //file_ext := Copy(file_ext, 2, Length(file_ext) - 1);

  // имя должно быть не длиннее 68 символом, поэтому лишнее обрежется
  file_name := Copy(file_name, 0, FILENAME_SIZE);

  // единичным байтом заполняем, потому что после ксора хвосты строк
  // снова станут нулевыми, так как по формату ксорится только содержательная часть
  FillMemory(@chunk, SizeOf(chunk), 1);

  // используем тот факт, что все имена хранятся в ANSI,
  // поэтому просто копируем строку в вектор байтов
  CopyMemory(@chunk.FileName, @file_name[1], Length(file_name));
  chunk.Offset := ArcStream.Position;
  chunk.Size := Stream.Size;
  // расширение — есть шестанцатиричное число, обозначающее тип файла

  FillMemory(@xor_base_4b, 4, XorValue);
  // по формату это поле не ксорится и его надо пропускать,
  // но это приведет к падению скорости ксора всего файла,
  // поэтому мы его сразу поксорим, и при ксоре всего файла-списка
  // поле примет нужный вид
  chunk.Extension := ExtToFileType(LowerCase(file_ext)) xor xor_base_4b;

  Result := chunk;
end;

function LoadListFromFile(AFolder: string; AFileName: string): TStringDynArray;
var
  list: TStringDynArray;
  fl: TStringList;
  i: Cardinal;
begin
  fl := TStringList.Create;
  fl.LoadFromFile(AFileName);

  SetLength(list, fl.Count);
  for i := 0 to fl.Count - 1 do
    list[i] := AFolder + fl[i];

  FreeAndNil(fl);

  Result := list;
end;

// сортируем список файлов учитывая, что "_" идёт раньше латиницы
procedure SortFileName(var List: TStringDynArray);
begin
  TArray.Sort<string>(List, TDelegatedComparer<string>.Construct(
    function(const Left, Right: string): Integer
    var
      // используем временные переменные для сравнения, чтобы не затрагивать
      // сам список
      temp_left, temp_right: string;
    begin
      // символ "!" всегда идет первым, поэтому заменяем им все "_"
      temp_left := TStringBuilder.Create(Left).Replace('_', '!').ToString;
      temp_right := TStringBuilder.Create(Right).Replace('_', '!').ToString;

      // теперь строки можно сравнивать как обычно
      Result := TComparer<string>.Default.Compare(temp_left, temp_right);
    end));
end;

var
  // операнд для ксора
  xor_base: Byte;
  // имя папки для запаковки и название архива, куда будет паковать
  folder_name, file_name: string;
  // список файлов для запаковки
  file_list: TStringDynArray;
  // запись в таблице
  chunk: TTableChunk;
  // файл-список, файл-архив и файл для запаковки
  lst_file, arc_file, in_file: TFileStream;
  // некосренное содержимое файла-списка
  uncrypt_lst : TMemoryStream;
  // счётчик для цикла
  i: Integer;
  // оригинальный порядок файлов в таблице
  table_file_list: TStringList;

begin
  try
    { TODO -oUser -cConsole Main : Insert code here }
    Writeln('Koihime Musou SNX packer v 1.00');
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

    if DirectoryExists(folder_name) then
      if (ParamCount > 2) and FileExists(ParamStr(3)) then
        file_list := LoadListFromFile(folder_name, ParamStr(3))
      else begin
        // получаем список всех файлов в папке и подпапках
        file_list := TDirectory.GetFiles(folder_name, '*.*', TSearchOption.soAllDirectories);
        //SortFileName(file_list);
      end
    else begin
      WriteLn('Folder doesn''t exist!');
      Exit;
    end;

    // по умолчанию будет единица
    xor_base := 1;

    // последним параметром может идти правый операнд для ксора
    if ParamCount > 2 then
      try
        // если число длинее одного байта, все старшие байты обрезаются
        xor_base := StrToInt(ParamStr(ParamCount));
        Writeln('Xor base: ', IntToStr(xor_base));
      except
        Writeln('If third parameter exist it must be value in range 0-255');
        Exit;
      end;

    // сюда будем писать содержимое таблицы
    lst_file := TFileStream.Create(ChangeFileExt(file_name, '.lst'), fmCreate);
    // сюда содержимое файлов
    arc_file := TFileStream.Create(file_name, fmCreate);
    // а тут будет храниться таблица в непоксоренном виде
    uncrypt_lst := TMemoryStream.Create;

    // первым делом добавляем кол-во записей в таблице
    i := Length(file_list);
    uncrypt_lst.Write(i, SizeOf(i));

    // поочереди добавляем информацию о всех файлах в таблицу,
    // а содержимое самих файлов в архив
    for i := 0 to Length(file_list) - 1 do
    begin
      Write('Add ', file_list[i], '... ');
      // добавляемый файл
      in_file := TFileStream.Create(file_list[i], fmOpenRead);

      chunk := MakeChunk(in_file, arc_file, xor_base);

      uncrypt_lst.Write(chunk, TABLE_CHUNK_SIZE);
      arc_file.CopyFrom(in_file, 0);

      FreeAndNil(in_file);
      Writeln('Done.');
    end;

    Write('Crypting... ');
    // ксорить надо весь файл, поэтому указатель ставим на начало файла
    uncrypt_lst.Position := 0;
    // по формату полагается xor файла-списка (по умолчанию 1)
    XorStream(uncrypt_lst, lst_file, xor_base);
    Writeln('Done.');

    FreeAndNil(uncrypt_lst);
    FreeAndNil(lst_file);
    FreeAndNil(arc_file);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
