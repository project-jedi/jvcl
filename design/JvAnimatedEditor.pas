{$I JVCL.INC}
{$I WINDOWSONLY.INC}
unit JvAnimatedEditor;

interface
uses
  Windows, Forms, Graphics, ImgList, JvxAnimate, JvAniFile, 
  {$IFDEF COMPILER6_UP}
  DesignIntf, DesignEditors, DesignMenus, VCLEditors,
  {$ELSE}
  DsgnIntf,
  {$ENDIF}
  SysUtils, Classes, Dialogs, Controls;

type
  TJvAnimatedEditor = class(TComponentEditor)
  private
    FContinue: Boolean;
    {$IFDEF COMPILER6_UP}
    procedure CheckEdit(const PropertyEditor: IProperty);
    {$ELSE}
    procedure CheckEdit(PropertyEditor: TPropertyEditor);
    {$ENDIF}
    procedure EditImage(Image: TJvAnimatedImage);
    procedure LoadAniFile(Image: TJvAnimatedImage);
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

implementation
uses
  JvxDConst, TypInfo;

//=== TJvAnimatedEditor ======================================================
  
{$IFDEF COMPILER6_UP}
procedure TJvAnimatedEditor.CheckEdit(const PropertyEditor: IProperty);
begin
{$ELSE}
procedure TJvAnimatedEditor.CheckEdit(PropertyEditor: TPropertyEditor);
begin
  try
{$ENDIF}
    if FContinue and (CompareText(PropertyEditor.GetName, 'GLYPH') = 0) then
    begin
      PropertyEditor.Edit;
      FContinue := False;
    end;
{$IFNDEF COMPILER6_UP}
  finally
    PropertyEditor.Free;
  end;
{$ENDIF}
end;

{$IFDEF COMPILER6_UP}
type
  TDesignerSelectionList = IDesignerSelections;
{$ENDIF}

procedure TJvAnimatedEditor.EditImage(Image: TJvAnimatedImage);
var
  Components: TDesignerSelectionList;
begin
  {$IFDEF COMPILER6_UP}
  Components := TDesignerSelections.Create;
  {$ELSE}
  Components := TDesignerSelectionList.Create;
  {$ENDIF}
  {$IFNDEF COMPILER6_UP}
  try
  {$ENDIF}
    FContinue := True;
    Components.Add(Component);
    GetComponentProperties(Components, tkAny, Designer, CheckEdit);
  {$IFNDEF COMPILER6_UP}
  finally
    Components.Free;
  end;
  {$ENDIF}
end;

procedure TJvAnimatedEditor.LoadAniFile(Image: TJvAnimatedImage);
var
  Dialog: TOpenDialog;
  AniCursor: TJvAnimatedCursorImage;
  CurDir: string;
begin
  CurDir := GetCurrentDir;
  Dialog := TOpenDialog.Create(Application);
  try
    with Dialog do
    begin
      Options := [ofHideReadOnly, ofFileMustExist];
      DefaultExt := 'ani';
      Filter := srAniCurFilter;
      if Execute then
      begin
        AniCursor := TJvAnimatedCursorImage.Create;
        try
          AniCursor.LoadFromFile(FileName);
          AniCursor.AssignToBitmap(Image.Glyph, clFuchsia, True,
            Image.Orientation = goVertical);
          Image.Interval := AniCursor.DefaultRate;
          Image.TransparentColor := clFuchsia;
          Designer.Modified;
        finally
          AniCursor.Free;
        end;
      end;
    end;
  finally
    Dialog.Free;
    SetCurrentDir(CurDir);
  end;
end;

procedure TJvAnimatedEditor.ExecuteVerb(Index: Integer);
begin
  if Index = GetVerbCount - 1 then
    LoadAniFile(TJvAnimatedImage(Component))
  else
  if Index = GetVerbCount - 2 then
    EditImage(TJvAnimatedImage(Component))
  else
    inherited ExecuteVerb(Index);
end;

function TJvAnimatedEditor.GetVerb(Index: Integer): string;
begin
  if Index = GetVerbCount - 1 then
    Result := srLoadAniCursor
  else
  if Index = GetVerbCount - 2 then
    Result := srEditPicture
  else
    Result := inherited GetVerb(Index);
end;

function TJvAnimatedEditor.GetVerbCount: Integer;
begin
  Result := inherited GetVerbCount + 2;
end;

end.
