unit JvRollOutEditor;
{$I jvcl.inc}
interface
uses
  {$IFDEF COMPILER6_UP}
  DesignIntf, DesignEditors,   
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
  TJvRollOutDefaultEditor = class(TDefaultEditor)
  public
    procedure Edit; override;
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

{ TJvRollOutDefaultEditor }

procedure TJvRollOutDefaultEditor.Edit;
var R:TJvRollOut;
begin
  if (GetComponent is TJvRollOut) then
  begin
    R := TJvRollOut(GetComponent);
    if R.MouseIsOnButton then
    begin
      R.Collapsed := not R.Collapsed;
      Designer.Modified;
    end;
  end;

end;

end.
