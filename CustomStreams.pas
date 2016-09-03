unit CustomStreams;

interface

uses
  Classes;

type
  TValueSize = (vsByte, vsWord, vsDWord);

  TXorStream = class(TMemoryStream)
  private
    FMask: Cardinal;
  protected
    procedure XorBuffer(const inBuffer; var outBuffer; Size: LongInt; Mask: Cardinal);
  public
    function Write(const Buffer; Count: Longint): Longint; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    property Mask: Cardinal read FMask write FMask;
  end;

implementation

{ TXorStream }

{$O-}
function TXorStream.Read(var Buffer; Count: Integer): Longint;
var
  xor_buf: array of Byte;
  buf_size: Cardinal;
begin
  // поскольку маска 4х битная, то и буфер должен кратен четырем
  // итого получим буфер по размеру не меньший необходимого и дополненный до 4х
  buf_size := (Count div 4 + 1) * 4;
  SetLength(xor_buf, buf_size);

  inherited Read(xor_buf[0], buf_size);

  // если буфер чтения оказался по размеру больше запрашиваемого,
  // надо сдвинуть указатель назад
  Seek(Count - buf_size, soFromCurrent);

  XorBuffer(xor_buf[0], xor_buf[0], buf_size, FMask);

  Move(xor_buf[0], Buffer, Count);

  SetLength(xor_buf, 0);

  Result := Count;
end;

function TXorStream.Write(const Buffer; Count: Integer): Longint;
var
  xor_buf: array of Byte;
  buf_size: Cardinal;
begin
  buf_size := (Count div 4 + 1) * 4;
  SetLength(xor_buf, buf_size);

  Move(Buffer, xor_buf[0], Count);

  XorBuffer(xor_buf[0], xor_buf[0], buf_size, FMask);

  Result := inherited Write(xor_buf[0], Count);

  SetLength(xor_buf, 0);
end;
{$O+}

procedure TXorStream.XorBuffer(const inBuffer; var outBuffer; Size: Integer;
  Mask: Cardinal);
Label
  Label_xor;

asm
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
end;

end.
