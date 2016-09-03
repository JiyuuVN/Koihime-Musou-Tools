program km_check_diff;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  IOUtils,
  Types,
  Classes,
  Windows,
  Generics.Collections,
  km_archive in 'km_archive.pas';

var
  // ��� ����� ��� ��������� � �������� ������, ���� ����� ��������
  folder_name, file_name: string;
  // ������ ������ ��� ���������
  file_list: TStringDynArray;
  // ������� ��� �����
  i, idx: Integer;
  // ������������ ������� ������ � �������
  table_file_list: TStringList;

begin
  try
    { TODO -oUser -cConsole Main : Insert code here }
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

    if FileExists(file_name) then
    begin
      table_file_list := TStringList.Create;
      table_file_list.LoadFromFile(file_name);
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

    file_list := TDirectory.GetFiles(folder_name, '*.*', TSearchOption.soAllDirectories);

    for i := 0 to Length(file_list) do
    begin
      idx := table_file_list.IndexOf(ExtractFileName(file_list[i]));
      if idx <> -1 then
        table_file_list.Delete(idx);
    end;

    table_file_list.SaveToFile(file_name + '.check');

    FreeAndNil(table_file_list);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
