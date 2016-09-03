unit km_snx;

interface

uses
  Classes,
  Generics.Collections;

const
  OP_READ_FROM_DBS = $11;
  VAL_READ_FROM_DBS = $02;

  CHR_PRINT_TEXT = #1;
  CHR_PRINT_TEXT_REPLACE = '$';
  CHR_TWO = #2;
  CHR_TWO_REPLACE = '^';
  CHR_THREE = #3;
  CHR_THREE_REPLACE = '%';

  STR_WAIT_FOR_INPUT = CHR_TWO + CHR_THREE;
  STR_WAIT_FOR_INPUT_REPLACE = '@';

  SNX_CRYPT_VALUE = $02;

type
  TSNXCommand = record
    OpCode, FValue, SValue: Cardinal;
  end;

  TSNXCode = TArray<TSNXCommand>;

  TDataAddr = TList<Cardinal>;

  TDataChunk = TArray<AnsiChar>;

function ThisDBSReadCommand(Command: TSNXCommand): Boolean;
function Xored(Stream: TStream): Boolean;

implementation

function ThisDBSReadCommand(Command: TSNXCommand): Boolean;
begin
  Result := (Command.OpCode = OP_READ_FROM_DBS) and (Command.FValue = VAL_READ_FROM_DBS);
end;

function Xored(Stream: TStream): Boolean;
var
  op_count, data_size: Cardinal;
begin
  Stream.Position := 0;
  Stream.Read(op_count, SizeOf(op_count));
  Stream.Read(data_size, SizeOf(data_size));

  Result := (8 + op_count * SizeOf(TSNXCommand) + data_size) <> Stream.Size;
end;

end.
