{$I JVCL.INC}

unit JvCryptReg;

interface

procedure Register;

implementation
uses
  Classes,
  JvConsts, JvVigenereCipher, JvCabFile, JvCaesarCipher, JvGenetic, JvSerialMaker,
  JvXorCipher
  {$IFNDEF BCB}
  , JvZlibMultiple
  {$ENDIF}
  ;
{$R ..\resources\JvCryptReg.dcr}

procedure Register;
begin
  RegisterComponents(SPaletteEncryptCompress,[
    TJvVigenereCipher, TJvXORCipher, TJvCaesarCipher, TJvGenetic,
    TJvCABFile
    {$IFNDEF BCB}
    , TJvZlibMultiple
    {$ENDIF}
    , TJvSerialMaker
    ]);
end;

end.
