{$I JVCL.INC}
{$I WINDOWSONLY.INC}
unit JvOutlookBarEditors;

interface
uses
  Windows, SysUtils, Classes, Controls, Forms, ToolWin,
  Menus, ActnList, ComCtrls, ImgList,
  {$IFDEF COMPILER6_UP}
  DesignEditors, DesignIntf, DesignMenus, DesignWindows,
  {$ELSE}
  DsgnIntf, DsgnWnds,
  {$ENDIF}
  JvDsgnEditors, JvOutlookBar;

type
  TJvOutlookBarActivePageEditor = class(TIntegerProperty)
  private
    function GetOL: TJvCustomOutlookBar;
  protected
    property OL: TJvCustomOutlookBar read GetOL;
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TJvOutlookBarComponentEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TJvOutlookBarPagesPropertyEditor = class(TPropertyEditor)
  private
    function GetOutlookBar: TJvCustomOutlookBar;
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
  end;

  TJvOutlookBarButtonImageIndexProperty = class(TJvDefaultImageIndexProperty)
  protected
    function GetPage: TJvOutlookBarPage;
    function GetBar: TJvCustomOutlookBar;
    function ImageList: TCustomImageList; override;
  end;

procedure Register;
  
implementation
uses
  JvOutlookBarForm;
  
type
  THackOutlookBar = class(TJvCustomOutlookBar);

resourcestring
  SOLEditor = 'OutlookBar Editor...';
  
procedure Register;
begin
  RegisterComponentEditor(TJvCustomOutlookBar, TJvOutlookBarComponentEditor);
  RegisterPropertyEditor(TypeInfo(Integer),
    TJvCustomOutlookBar, 'ActivePageIndex', TJvOutlookBarActivePageEditor);
  RegisterPropertyEditor(TypeInfo(TJvOutlookBarPages),
    TJvCustomOutlookBar, '', TJvOutlookBarPagesPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TJvOutlookBarButtons),
    TJvOutlookBarPage, '', TJvOutlookBarPagesPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer),
    TJvOutlookBarButton, 'ImageIndex', TJvOutlookBarButtonImageIndexProperty);
end;


procedure ShowEditor(Designer: IDesigner; OutlookBar: TJvCustomOutlookBar);
var
  I: Integer;
  AEditor: TfrmOLBEditor;
begin
  AEditor := nil;
  for I := 0 to Screen.FormCount - 1 do
  begin
    if Screen.Forms[I] is TFrmOLBEditor then
    begin
      if TFrmOLBEditor(Screen.Forms[I]).OutlookBar = OutlookBar then
      begin
        AEditor := TFrmOLBEditor(Screen.Forms[I]);
        Break;
      end;
    end;
  end;
  // Show the editor
  if Assigned(AEditor) then
  begin
    AEditor.Show;
    if AEditor.WindowState = wsMinimized then
      AEditor.WindowState := wsNormal;
  end
  else
  begin
    AEditor := TFrmOLBEditor.Create(Application);
    try
      {$IFDEF COMPILER6_UP}
      AEditor.Designer := Designer;
      {$ELSE}
      AEditor.Designer := Designer as IFormDesigner;
      {$ENDIF}
      AEditor.OutlookBar := OutlookBar;
      AEditor.Show;
    except
      AEditor.Free;
      raise;
    end;
  end;
end;

//=== TJvOutlookBarPagesPropertyEditor =======================================

procedure TJvOutlookBarPagesPropertyEditor.Edit;
begin
  ShowEditor(Designer, GetOutlookBar);
end;

function TJvOutlookBarPagesPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

type
  THackPages = class(TJvOutlookBarPages);
  THackButtons = class(TjvOutlookBarButtons);

function TJvOutlookBarPagesPropertyEditor.GetOutlookBar: TJvCustomOutlookBar;
begin
  if GetComponent(0) is TJvCustomOutlookBar then
    Result := TJvCustomOutlookBar(GetComponent(0))
  else
    if GetComponent(0) is TJvOutlookBarPage then
    Result := THackOutlookBar(THackPages(TJvOutlookBarPage(GetComponent(0)).Collection).GetOwner)
  else
    Result := nil;
end;

function TJvOutlookBarPagesPropertyEditor.GetValue: string;
begin
  Result := Format('(%s)', [GetPropType^.Name]);
end;

//=== TJvOutlookBarComponentEditor ===========================================

procedure TJvOutlookBarComponentEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0:
      ShowEditor(Designer, Component as TJvCustomOutlookBar);
  else
    inherited ExecuteVerb(Index);
  end;
end;

function TJvOutlookBarComponentEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:
      Result := SOLEditor;
  else
    Result := inherited GetVerb(Index);
  end;
end;

function TJvOutlookBarComponentEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

//=== TJvOutlookBarActivePageEditor ==========================================

procedure TJvOutlookBarActivePageEditor.Edit;
begin
  inherited Edit;
end;

function TJvOutlookBarActivePageEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paRevertable];
end;

function TJvOutlookBarActivePageEditor.GetOL: TJvCustomOutlookBar;
begin
  if GetComponent(0) is TJvCustomOutlookBar then
    Result := TJvCustomOutlookBar(GetComponent(0))
  else
    Result := nil;
end;

function TJvOutlookBarActivePageEditor.GetValue: string;
var
  I: Integer;
begin
  I := GetOrdValue;
  if I < 0 then
    Result := ''
  else
    if I < THackOutlookBar(OL).Pages.Count then
    Result := THackOutlookBar(OL).Pages[I].Caption
  else
    Result := inherited GetValue;
end;

procedure TJvOutlookBarActivePageEditor.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  for I := 0 to THackOutlookBar(OL).Pages.Count - 1 do
    Proc(THackOutlookBar(OL).Pages[I].Caption);
end;

procedure TJvOutlookBarActivePageEditor.SetValue(const Value: string);
var
  I: Integer;
begin
  I := StrToIntDef(Value, -1);
  if I < 0 then
  begin
    for I := 0 to THackOutlookBar(OL).Pages.Count - 1 do
      if AnsiSameText(THackOutlookBar(OL).Pages[I].Caption, Value) then
      begin
        SetOrdValue(I);
        Modified;
        Exit;
      end;
  end
  else
    inherited SetValue(Value);
end;

//=== TJvOutlookBarButtonImageIndexProperty ==================================

function TJvOutlookBarButtonImageIndexProperty.GetBar: TJvCustomOutlookBar;
begin
  Result := THackPages(GetPage.Collection).GetOwner as TJvCustomOutlookBar;
end;

function TJvOutlookBarButtonImageIndexProperty.GetPage: TJvOutlookBarPage;
begin
  Result := TJvOutlookBarPage(THackButtons((GetComponent(0) as TJvOutlookBarButton).Collection).GetOwner);
end;

function TJvOutlookBarButtonImageIndexProperty.ImageList: TCustomImageList;
begin
  if GetPage.ButtonSize = olbsLarge then
    Result := THackOutlookBar(GetBar).LargeImages
  else
    Result := THackOutlookBar(GetBar).SmallImages;
end;


end.
