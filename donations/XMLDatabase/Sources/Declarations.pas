unit Declarations;

interface


uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics;

type
  PFixedFileInfo = ^TFixedFileInfo;
  TFixedFileInfo = record
     Signature: DWord;
     StrucVersion: DWord;
     Minor: Word;
     Major: Word;
     Build: Word;
     Release: Word;
     FileFlagsMask: DWord;
     FileFlags: DWord;
     FileOS: DWord;
     FileType: DWord;
     FileSubtype: DWord;
     FileDateMS: DWord;
     FileDateLS: DWord;
  end;

implementation

end.

