unit UpdBckgrnd;

interface

procedure HandleFile(const InName: string);

implementation

uses
  SysUtils, Classes, Graphics,
  pngimage,
  genimage_main;

procedure MakePNGAndSave(const BMP: TBitmap; const FileName: string);
var
  PNG: TPngObject;
begin
  PNG := TPNGObject.Create;
  try
    PNG.Assign(BMP);
    PNG.CompressionLevel := 9;
    PNG.SaveToFile(FileName); 
  finally
    PNG.Free;
  end;
end;

procedure HandleFile(const InName: string);
var
  BMP: TBitmap;
  Col: TColor;
  X: Integer;
  Y: Integer;
begin
  WriteLn('  file: ', ExtractFileName(InName));
  BMP := TBitmap.Create;
  try
    BMP.LoadFromFile(InName);
    BMP.TransparentMode := tmAuto;
    Col := BMP.Canvas.Pixels[0, BMP.Height - 1];
    for Y := 0 to BMP.Height - 1 do
    begin
      for X := 0 to BMP.Width - 1 do
      begin
        if BMP.Canvas.Pixels[X, Y] = Col then
          BMP.Canvas.Pixels[X, Y] := clBtnFace;
      end;
    end;
    // Save new file
    BMP.SaveToFile(OutPath + ExtractFileName(InName));
    // Transform to PNG and save
    MakePNGAndSave(BMP, OutPath + ChangeFileExt(ExtractFileName(InName), '.png'));
  finally
    BMP.Free;
  end;
end;

end.
