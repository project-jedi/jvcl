unit JvPageListTreeViewReg;
interface
uses
  Windows, Classes, JvPageListTreeView, JvDsgnEditors,
  DesignEditors, DesignIntf, DesignMenus, VCLEditors, ImgList,
  Graphics;

type

  { a property editor for the ACtivePage property of TJvPageList }
  TJvActivePageProperty = class(TComponentProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TJvShowDesignCaptionProperty = class(TEnumProperty)
    function GetAttributes: TPropertyAttributes; override;
  end;

  { a component editor for the TJvPageList }
  TJvCustomPageEditor = class(TComponentEditor)
  private
    procedure InsertPage;
    procedure PrevPage;
    procedure NextPage;
    procedure RemovePage;
    function GetPageControl: TJvCustomPageList;
  public
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;
    procedure Edit; override;
    procedure PrepareItem(Index: Integer; const AItem: IMenuItem);
      override;
  end;

  TJvSettingsTreeImagesProperty = class(TJvDefaultImageIndexProperty)
  protected
    function ImageList: TCustomImageList; override;
  end;

procedure Register;
{$R ..\resources\JvPageListTreeViewReg.dcr}

implementation
uses
  Forms, ComCtrls, Controls, SysUtils,
  JvPageLinkEditor, JvTreeItemsEditor, TypInfo;

type
  THackTreeView = class(TJvCustomPageListTreeView);

procedure Register;
begin
  RegisterPropertyEditor(typeinfo(TTreeNodes),TCustomTreeView,'Items',TJvTreeItemsProperty);
  RegisterPropertyEditor(typeinfo(TJvShowDesignCaption),nil,'',TJvShowDesignCaptionProperty);
  RegisterComponents('JVCL', [TJvSettingsTreeView, TJvPageListTreeView, TJvPageList]);
  RegisterClasses([TJvSettingsTreeView, TJvPageListTreeView, TJvPageList, TJvStandardPage]);
  RegisterComponentEditor(TJvCustomPageList, TJvCustomPageEditor);
  RegisterComponentEditor(TJvCustomPage, TJvCustomPageEditor);
  RegisterComponentEditor(TCustomTreeView, TJvTreeViewComponentEditor);
  // register for the standard TTreeView as well
  RegisterComponentEditor(TTreeView, TJvTreeViewComponentEditor);
  RegisterPropertyEditor(typeinfo(TJvPageLinks),
    TJvCustomPageListTreeView, '', TJvPageLinksProperty);
  RegisterPropertyEditor(typeinfo(TJvCustomPage),
    TJvCustomPageList, 'ActivePage', TJvActivePageProperty);
  RegisterPropertyEditor(typeinfo(TImageIndex),TJvSettingsTreeImages,'',TJvSettingsTreeImagesProperty);
//  RegisterPropertyEditor(typeinfo(integer),TJvSettingsTreeImages,'CollapsedIndex',TJvSettingsTreeImagesProperty);
//  RegisterPropertyEditor(typeinfo(integer),TJvSettingsTreeImages,'ExpandedIndex',TJvSettingsTreeImagesProperty);
//  RegisterPropertyEditor(typeinfo(integer),TJvSettingsTreeImages,'ImageIndex',TJvSettingsTreeImagesProperty);
//  RegisterPropertyEditor(typeinfo(integer),TJvSettingsTreeImages,'SelectedIndex',TJvSettingsTreeImagesProperty);
end;


{ TJvCustomPageEditor }

procedure TJvCustomPageEditor.Edit;
begin
  // do nothing
end;

procedure TJvCustomPageEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: InsertPage;
    1: NextPage;
    2: PrevPage;
    3: RemovePage;
  end;
end;

function TJvCustomPageEditor.GetPageControl: TJvCustomPageList;
begin
  if Component is TJvCustomPageList then
    Result := TJvCustomPageList(Component)
  else
    Result := TJvCustomPageList(TJvCustomPage(Component).PageList);
end;

function TJvCustomPageEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := '&New Page';
    1: Result := 'Ne&xt Page';
    2: Result := '&Previous Page';
    3: Result := '&Delete Page';
    4: Result := '&Move from PageControl...';
  end;
end;

function TJvCustomPageEditor.GetVerbCount: Integer;
begin
  Result := 4;                          // new, next, previous, delete,
end;

procedure TJvCustomPageEditor.InsertPage;
var P: TJvCustomPage;
  C: TJvCustomPageList;
begin
  C := GetPageControl;
  P := C.GetPageClass.Create(Designer.Root);
  try
    P.Parent := C;
    P.Name := Designer.UniqueName(C.GetPageClass.ClassName);
    P.PageList := C;
    C.ActivePage := P;
  except
    P.Free;
    raise;
  end;
end;

procedure TJvCustomPageEditor.NextPage;
begin
  GetPageControl.NextPage;
end;

procedure TJvCustomPageEditor.PrepareItem(Index: Integer;
  const AItem: IMenuItem);
begin
  inherited;
  if (Index = 3) and (GetPageControl.ActivePage = nil) then
    AItem.Enabled := false;
end;

procedure TJvCustomPageEditor.PrevPage;
begin
  GetPageControl.PrevPage;
end;

procedure TJvCustomPageEditor.RemovePage;
var P: TJvCustomPageList;P2:TJvCustomPage;
begin
  P := GetPageControl;
  if (P <> nil) and (P.ActivePage <> nil) then
  begin
    P2 := P.ActivePage;
    Designer.SelectComponent(P2);
    P2.PageList := nil;
    P2.Free;
    Designer.Modified;
  end;
end;

{ TJvActivePageProperty }

function TJvActivePageProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

procedure TJvActivePageProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  Component: TComponent;
begin
  for I := 0 to Designer.GetRoot.ComponentCount - 1 do
  begin
    Component := Designer.GetRoot.Components[I];
    if (Component.Name <> '') and (Component is TJvCustomPage) and
      (TJvCustomPage(Component).PageList = GetComponent(0)) then
      Proc(Component.Name);
  end;
end;

{ TJvSettingsTreeImagesProperty }

function TJvSettingsTreeImagesProperty.ImageList: TCustomImageList;
var T:TJvCustomPageListTreeView;
begin
  if (GetComponent(0) is TJvSettingsTreeImages) and (TJvSettingsTreeImages(GetComponent(0)).TreeView <> nil) then
  begin
    T := TJvSettingsTreeImages(GetComponent(0)).TreeView;
    Result :=  THackTreeView(T).Images;
  end
  else
   Result := nil;
end;

{ TJvShowDesignCaptionProperty }

function TJvShowDesignCaptionProperty.GetAttributes: TPropertyAttributes;
begin
  // we don't want sorting for this property
  Result := [paMultiSelect, paValueList, paRevertable];
end;

end.

