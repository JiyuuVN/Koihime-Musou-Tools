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

// ��������� �� ������� ������������ ����� � �����-������ ��� �����������
// ���� ��� ������ � �������
function MakeChunk(Stream: TFileStream; ArcStream: TStream; XorValue: Byte =
    1): TTableChunk;
var
  chunk: TTableChunk;
  file_name: AnsiString;
  file_ext: AnsiString;
  // 4 �������� ����� ��� ����� ���� �����
  xor_base_4b: Cardinal;
begin
  file_name := ExtractFileName(Stream.FileName);
  file_ext := ExtractFileExt(Stream.FileName);

  // � ������� �������� ������ ����� ��� �����, ������� �������� ����������
  file_name := Copy(file_name, 0, Length(file_name) - Length(file_ext));
  // ������� �� ���������� �����
  //file_ext := Copy(file_ext, 2, Length(file_ext) - 1);

  // ��� ������ ���� �� ������� 68 ��������, ������� ������ ���������
  file_name := Copy(file_name, 0, FILENAME_SIZE);

  // ��������� ������ ���������, ������ ��� ����� ����� ������ �����
  // ����� ������ ��������, ��� ��� �� ������� �������� ������ �������������� �����
  FillMemory(@chunk, SizeOf(chunk), 1);

  // ���������� ��� ����, ��� ��� ����� �������� � ANSI,
  // ������� ������ �������� ������ � ������ ������
  CopyMemory(@chunk.FileName, @file_name[1], Length(file_name));
  chunk.Offset := ArcStream.Position;
  chunk.Size := Stream.Size;
  // ���������� � ���� ���������������� �����, ������������ ��� �����

  FillMemory(@xor_base_4b, 4, XorValue);
  // �� ������� ��� ���� �� �������� � ��� ���� ����������,
  // �� ��� �������� � ������� �������� ����� ����� �����,
  // ������� �� ��� ����� ��������, � ��� ����� ����� �����-������
  // ���� ������ ������ ���
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

// ��������� ������ ������ ��������, ��� "_" ��� ������ ��������
procedure SortFileName(var List: TStringDynArray);
begin
  TArray.Sort<string>(List, TDelegatedComparer<string>.Construct(
    function(const Left, Right: string): Integer
    var
      // ���������� ��������� ���������� ��� ���������, ����� �� �����������
      // ��� ������
      temp_left, temp_right: string;
    begin
      // ������ "!" ������ ���� ������, ������� �������� �� ��� "_"
      temp_left := TStringBuilder.Create(Left).Replace('_', '!').ToString;
      temp_right := TStringBuilder.Create(Right).Replace('_', '!').ToString;

      // ������ ������ ����� ���������� ��� ������
      Result := TComparer<string>.Default.Compare(temp_left, temp_right);
    end));
end;

var
  // ������� ��� �����
  xor_base: Byte;
  // ��� ����� ��� ��������� � �������� ������, ���� ����� ��������
  folder_name, file_name: string;
  // ������ ������ ��� ���������
  file_list: TStringDynArray;
  // ������ � �������
  chunk: TTableChunk;
  // ����-������, ����-����� � ���� ��� ���������
  lst_file, arc_file, in_file: TFileStream;
  // ����������� ���������� �����-������
  uncrypt_lst : TMemoryStream;
  // ������� ��� �����
  i: Integer;
  // ������������ ������� ������ � �������
  table_file_list: TStringList;

begin
  try
    { TODO -oUser -cConsole Main : Insert code here }
    Writeln('Koihime Musou SNX packer v 1.00');
    Writeln('Author: HeMet');
    Writeln('Jiyuu-VN: http://jiyuu-vn.ru');
    // ���������� ������ ���� ��� ������� ���
    if ParamCount < 2 then
    begin
      Writeln('Parameters count must be at least 2!');
      Exit;
    end;

    // ������ ���� �������� ����� ��� ���������
    folder_name := ParamStr(1);
    // ������ ��� ������, ���� ����� ��������
    file_name := ParamStr(2);

    if DirectoryExists(folder_name) then
      if (ParamCount > 2) and FileExists(ParamStr(3)) then
        file_list := LoadListFromFile(folder_name, ParamStr(3))
      else begin
        // �������� ������ ���� ������ � ����� � ���������
        file_list := TDirectory.GetFiles(folder_name, '*.*', TSearchOption.soAllDirectories);
        //SortFileName(file_list);
      end
    else begin
      WriteLn('Folder doesn''t exist!');
      Exit;
    end;

    // �� ��������� ����� �������
    xor_base := 1;

    // ��������� ���������� ����� ���� ������ ������� ��� �����
    if ParamCount > 2 then
      try
        // ���� ����� ������ ������ �����, ��� ������� ����� ����������
        xor_base := StrToInt(ParamStr(ParamCount));
        Writeln('Xor base: ', IntToStr(xor_base));
      except
        Writeln('If third parameter exist it must be value in range 0-255');
        Exit;
      end;

    // ���� ����� ������ ���������� �������
    lst_file := TFileStream.Create(ChangeFileExt(file_name, '.lst'), fmCreate);
    // ���� ���������� ������
    arc_file := TFileStream.Create(file_name, fmCreate);
    // � ��� ����� ��������� ������� � ������������� ����
    uncrypt_lst := TMemoryStream.Create;

    // ������ ����� ��������� ���-�� ������� � �������
    i := Length(file_list);
    uncrypt_lst.Write(i, SizeOf(i));

    // ��������� ��������� ���������� � ���� ������ � �������,
    // � ���������� ����� ������ � �����
    for i := 0 to Length(file_list) - 1 do
    begin
      Write('Add ', file_list[i], '... ');
      // ����������� ����
      in_file := TFileStream.Create(file_list[i], fmOpenRead);

      chunk := MakeChunk(in_file, arc_file, xor_base);

      uncrypt_lst.Write(chunk, TABLE_CHUNK_SIZE);
      arc_file.CopyFrom(in_file, 0);

      FreeAndNil(in_file);
      Writeln('Done.');
    end;

    Write('Crypting... ');
    // ������� ���� ���� ����, ������� ��������� ������ �� ������ �����
    uncrypt_lst.Position := 0;
    // �� ������� ���������� xor �����-������ (�� ��������� 1)
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
