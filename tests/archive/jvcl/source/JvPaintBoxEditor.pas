{$I JVCL.INC}
{$I WINDOWSONLY.INC}
unit JvPaintBoxEditor;

interface
uses
  Windows, Forms, Graphics, ImgList,
  {$IFDEF COMPILER6_UP}
  DesignIntf, DesignEditors, DesignMenus, VCLEditors,
  {$ELSE}
  DsgnIntf,
  {$ENDIF}
  SysUtils, Classes, Dialogs, Controls;

//=== TJvPaintBoxEditor ======================================================

type
  TJvPaintBoxEditor = class(TDefaultEditor)
  public
    {$IFDEF COMPILER6_UP}
    procedure EditProperty(const PropertyEditor: IProperty;
      var Continue: Boolean); override;
    {$ELSE}
    procedure EditProperty(PropertyEditor: TPropertyEditor;
      var Continue, FreeEditor: Boolean); override;
    {$ENDIF}
  end;

implementation

{$IFDEF COMPILER6_UP}
procedure TJvPaintBoxEditor.EditProperty(const PropertyEditor: IProperty;
  var Continue: Boolean);
{$ELSE}
procedure TJvPaintBoxEditor.EditProperty(PropertyEditor: TPropertyEditor;
  var Continue, FreeEditor: Boolean);
{$ENDIF}
begin
  if CompareText(PropertyEditor.GetName, 'OnPaint') = 0 then
  begin
    PropertyEditor.Edit;
    Continue := False;
  end
  else
    inherited;
end;

end.
 
