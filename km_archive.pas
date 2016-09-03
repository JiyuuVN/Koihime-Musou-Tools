unit km_archive;

interface

uses
  Classes, Windows;

const
  // ������ ������ � �������
  TABLE_CHUNK_SIZE = 76;
  // ����� ����� �����
  FILENAME_SIZE = 64;
  // �������� ��� ���������� ��������� �� ���������
  // (����� ����� ������ ���������� 00 00 00 04)
  EXT_DEFAULT = $00000000;

  KM_TYPE_SNX = $00000001;
  KM_EXT_SNX = '.snx';
  KM_TYPE_BMP = $00000002;
  KM_EXT_BMP = '.bmp';
  KM_TYPE_PNG = $00000003;
  KM_EXT_PNG = '.png';
  KM_TYPE_WAV = $00000004;
  KM_EXT_WAV = '.wav';
  KM_TYPE_OGG = $00000005;
  KM_EXT_OGG = '.ogg';

type
  // ��� ����� � �������: ANSI
  TTableFileName = array [0..FILENAME_SIZE - 1] of AnsiChar;

  // ��������� ������ � �������
  TTableChunk = record
    // �������� ����� �� ������ �����-������
    Offset: Cardinal;
    // ��� ������
    Size: Cardinal;
    // � ���
    FileName: TTableFileName;
    // ����� ����� ������������ ����������, ����� ����� ������ ���� 00 00 00 04
    Extension: Cardinal;
  end;

procedure XorStream(Input, Output: TStream; Value: Byte);
function FileTypeToExt(FileType: Cardinal): string;
function ExtToFileType(Ext: string): Cardinal;

implementation

procedure XorBuffer(const inBuffer; var outBuffer; Size: Integer; Mask: Cardinal);
Label
  Label_xor;

asm
  pushad
  mov ebx, outBuffer
  mov edx, inBuffer
  mov ecx, Size

Label_xor:
  mov eax, DWORD PTR [edx + ecx - 4]
  xor eax, Mask
  mov DWORD PTR [ebx + ecx - 4], eax
  sub ecx, 4
  cmp ecx, 0
  jnz Label_xor;
  popad
end;

// ������ ����� �� Value
procedure XorStream(Input, Output: TStream; Value: Byte);
const
  BUFFER_SIZE = $FF00;

var
  buffer: array[0.. BUFFER_SIZE - 1] of Byte;
  xor_base, byte_read: Integer;
  i: Int64;
begin
  // ��� ��������� ���������� ����� � 64 ����,
  // ������� ��������� ��������� ��� ��������� Value
  FillMemory(@xor_base, SizeOf(xor_base), Value);

  i := Input.Size;

  // ���� � ����������� �������� �������, ��� � �����������
  while i > 0 do
  begin
    byte_read := Input.Read(buffer[0], BUFFER_SIZE);
    XorBuffer(buffer[0], buffer[0], BUFFER_SIZE, xor_base);
    Output.Write(buffer[0], byte_read);
    dec(i, byte_read);
  end;

  // � ����� ������ ���������� ����� � �� ���� ������� 64 �����,
  // ������� ����������� �� ��������� �������
  Output.Size := Input.Size;
end;

function FileTypeToExt(FileType: Cardinal): string;
begin
  case FileType of
    KM_TYPE_SNX: Result := KM_EXT_SNX;
    KM_TYPE_BMP: Result := KM_EXT_BMP;
    KM_TYPE_PNG: Result := KM_EXT_PNG;
    KM_TYPE_WAV: Result := KM_EXT_WAV;
    KM_TYPE_OGG: Result := KM_EXT_OGG;
  else
    Result := '';
  end
end;

function ExtToFileType(Ext: string): Cardinal;
begin
  Result := EXT_DEFAULT;
  if Ext = KM_EXT_SNX then Result := KM_TYPE_SNX;
  if Ext = KM_EXT_BMP then Result := KM_TYPE_BMP;
  if Ext = KM_EXT_PNG then Result := KM_TYPE_PNG;
  if Ext = KM_EXT_WAV then Result := KM_TYPE_WAV;
  if Ext = KM_EXT_OGG then Result := KM_TYPE_OGG;
end;

end.
