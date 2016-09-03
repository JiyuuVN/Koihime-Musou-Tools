program km_decrypt;

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
  // ������� ��� �����
  xor_base: Byte;
  // 4 �������� ������� ��� ����� �������� �������� � �������
  xor_base_4b: Cardinal;
  // ��� ����� ��� ��������� � �������� ������, ���� ����� ��������
  folder_name, file_name: string;
  // ������ ������ ��� ���������
  file_list: TStringDynArray;
  // ������ � �������
  chunk: TTableChunk;
  // ����-������, ����-����� � ��������������� ����
  lst_file, arc_file, out_file: TFileStream;
  // ����������� ���������� �����-������
  uncrypt_lst : TMemoryStream;
  // ������� ��� �����
  i: Integer;
  // ������������ ������� ������ � �������
  table_file_list: TStringList;
  // ���-�� ������ � ������
  file_count: Cardinal;
  new_file_name: string;
begin
  try
    { TODO -oUser -cConsole Main : Insert code here }
    { TODO -oUser -cConsole Main : Insert code here }
    // ���������� ������ ���� ��� ������� ���
    if ParamCount < 1 then
    begin
      Writeln('Parameters count must be at least 2!');
      Exit;
    end;

    // ������ ��� ������, ���� ����� ��������
    file_name := ParamStr(1);

    if FileExists(file_name) then
    begin
      lst_file := TFileStream.Create(ChangeFileExt(file_name, '.lst'), fmOpenRead);
    end
    else begin
      WriteLn('File doesn''t exist!');
      Exit;
    end;

    xor_base := 1;

    Write('Uncrypting... ');
    uncrypt_lst := TMemoryStream.Create;
    XorStream(lst_file, uncrypt_lst, xor_base);
    uncrypt_lst.Position := 0;
    Writeln('Done.');

    uncrypt_lst.SaveToFile(file_name + '.uncrypted');

    FreeAndNil(uncrypt_lst);
    FreeAndNil(lst_file);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.

