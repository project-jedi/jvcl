unit ColorSpaceBmp;

interface

uses
    Windows, Classes, Graphics;

type
    TCsbVersion = ( cvNoCompress, cvRLE, cvSpot );

    TCsbBitPerPel = ( cbpp1, cbpp2, cbpp4, cbpp8, cbpp16, cbpp24 );

    TCsbHeader = packed record
               Name : array [0..2] of Char;
               Version : TCsbVersion;
               Compression : Word;
               BitPerPel : TCsbBitPerPel;
               Width : LongInt;
               Height : LongInt;
    end;

    TCsbPaletteEntry = record
                     Axe0 : Byte;
                     Axe1 : Byte;
                     Axe2 : Byte;
    end;

    TCsbPalette = packed record
                Size : Word;
                Entries : array [0..0] of TCsbPaletteEntry;
    end;

    TCsbData = packed record
             Data : array [0..0] of Byte;
    end;

//    TColorSpaceBitmap = class (TGraphic)
//    private
//           { Déclarations privées }
//    protected
//             { Déclarations protégées }
//             procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;
//             function Equals(Graphic: TGraphic): Boolean; override;
//             function GetEmpty: Boolean; override;
//             function GetHeight: Integer; override;
//             function GetPalette: HPALETTE; override;
//             function GetTransparent: Boolean; override;
//             function GetWidth: Integer; override;
//             procedure Progress(Sender: TObject; Stage: TProgressStage;
//                       PercentDone: Byte;  RedrawNow: Boolean; const R: TRect; const Msg: string); override;
//             procedure ReadData(Stream: TStream); override;
//             procedure SetHeight(Value: Integer); override;
//             procedure SetPalette(Value: HPALETTE); override;
//             procedure SetWidth(Value: Integer); override;
//             procedure WriteData(Stream: TStream); override;
//    public
//          { Déclarations publiques }
//          constructor Create; override;
//          procedure LoadFromStream(Stream: TStream); override;
//          procedure SaveToStream(Stream: TStream); override;
//          procedure LoadFromClipboardFormat(AFormat: Word; AData: THandle;
//                    APalette: HPALETTE); override;
//          procedure SaveToClipboardFormat(var AFormat: Word; var AData: THandle;
//                    var APalette: HPALETTE); override;
//    end;

const
     csb_Ext  = 'csb';
     csb_Name = 'Color Space Bitmap';

implementation

initialization

//TPicture.RegisterFileFormat(csb_Ext,csb_Name,TColorSpaceBitmap);

end.