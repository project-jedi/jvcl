{$I JVCL.INC}

unit JvCryptReg;

interface

procedure Register;

implementation
uses
  Classes,
  JvConsts, JvVigenereCipher, JvCabFile, JvCaesarCipher, JvGenetic, JvSerialMaker,
  JvXorCipher, JvZlibMultiple;
{$R ..\resources\JvCryptReg.dcr}

procedure Register;
begin
  RegisterComponents(SPaletteEncryptCompress,[
    TJvVigenereCipher, TJvXorCipher, TJvCaesarCipher, TJvGenetic,
    TJvCabFile, TJvZlibMultiple, TJvSerialMaker
    ]);
end;

end.
