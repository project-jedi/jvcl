unit JvRollOutEditor;
{$I jvcl.inc}
interface
uses
  {$IFDEF COMPILER6_UP}
  DesignIntf,  
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  ImgList,
  JvDsgnEditors;

type
// property editor for IndexCollapsed and IndexExpanded on a TJvRollOut to
// display the images from the imagelist and allow multiselect
  TJvRollOutOptionsImagesProperty = class(TJvDefaultImageIndexProperty)
  protected
    function ImageList: TCustomImageList; override;
  public
    function GetAttributes: TPropertyAttributes; override;
  end;

implementation
uses
  JvRollOut;

{ TJvRollOutOptionsImagesProperty }

function TJvRollOutOptionsImagesProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paMultiSelect];
end;

function TJvRollOutOptionsImagesProperty.ImageList: TCustomImageList;
begin
  Result := TJvRollOutImageOptions(GetComponent(0)).Images;
end;

end.
