{$I JVCL.INC}
unit JvTransparentButtonEditors;

interface
uses
  JvDsgnEditors,
  {$IFDEF COMPILER6_UP}
  DesignIntf, DesignEditors, DesignMenus, VCLEditors,
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  ImgList;


type
  TJvTBImagesProperty = class(TJvDefaultImageIndexProperty)
  protected
    function ImageList: TCustomImageList; override;
  end;

implementation
uses
  SysUtils, JvTransparentButton;

{ TJvTBImagesProperty }

function TJvTBImagesProperty.ImageList: TCustomImageList;
begin
  if AnsiSameText(GetName,'ActiveIndex') then
    Result := (GetComponent(0) as TJvTransparentButton2).ActiveImage
  else if AnsiSameText(GetName,'DisabledIndex') then
    Result := (GetComponent(0) as TJvTransparentButton2).DisabledImage
  else if AnsiSameText(GetName,'DownIndex') then
    Result := (GetComponent(0) as TJvTransparentButton2).DownImage
  else if AnsiSameText(GetName,'GrayIndex') then
    Result := (GetComponent(0) as TJvTransparentButton2).GrayImage
  else
    Result := nil;
end;

end.
