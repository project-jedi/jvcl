unit geCList;

interface
{$I glDEF.INC}
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, dsgnintf,
  StdCtrls, glPropCn, ComCtrls, ExtCtrls, TypInfo, Buttons {$IFDEF GLVER_D5}, ImgList{$ENDIF};

type

  TglComponentListProperty = class( TPropertyEditor )
    function GetAttributes : TPropertyAttributes; override;
    function GetValue : string; override;
    procedure Edit; override;
  end;

  TglComponentListEditor = class(TComponentEditor)
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;


  TglCompListEditor = class(TForm)
    lvAll: TListView;
    Label1: TLabel;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Label2: TLabel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    lvSel: TListView;
    pbAdd: TBitBtn;
    pbRemove: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    ImageList1: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lvAllDblClick(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure lvSelDblClick(Sender: TObject);
    procedure lvSelChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure lvAllChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
  private
    ComponentList: TStringList;
  public
    Component: TglPropertyCenter;
  end;

{$IFDEF GLVER_D4}
type
  TDesigner = IDesigner;
  TFormDesigner = IFormDesigner;
{$ENDIF}

var
  glCompListEditor: TglCompListEditor;

implementation
{$R *.DFM}
//----------- common proc
procedure ShowCompListEditor(Designer: TDesigner; glPropertyCenter: TglPropertyCenter);
var
  Dialog : TglCompListEditor;
  I : Integer;
begin
  Dialog := TglCompListEditor.Create( Application );
  Dialog.Component := glPropertyCenter;//TglPropertyCenter(GetComponent(0));
  Dialog.ShowModal;
  Dialog.free;
end;

//----------- TglComponentListProperty
function TglComponentListProperty.GetAttributes : TPropertyAttributes;
begin
  Result := [ paDialog ];
end;

function TglComponentListProperty.GetValue : string;
begin
  Result := Format( '(%s)', [ GetPropType^.Name ] );
end;

procedure TglComponentListProperty.Edit;
begin
  ShowCompListEditor(Designer, TglPropertyCenter(GetComponent(0)));
//  GetComponent(0).Owner.Name
end;
//----------- TglComponentListEditor
procedure TglComponentListEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: ShowCompListEditor(Designer, TglPropertyCenter(Component));
  end;
end;

function TglComponentListEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Edit component list...';
  end;
end;

function TglComponentListEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

//----------- TglCompListEditor
procedure TglCompListEditor.FormCreate(Sender: TObject);
begin
//  ControlsList := TList.create;
end;

procedure TglCompListEditor.FormDestroy(Sender: TObject);
begin
//  ControlsList.Free;
end;

procedure TglCompListEditor.FormShow(Sender: TObject);
var
  i, j: integer;
  ListItem: TListItem;
  Comp: TComponent;
  ColorPropInfo, FontPropInfo: PPropInfo;
const
  aSigns: array [boolean] of string = ('-', '+');
begin
  lvAll.Items.Clear;
  lvSel.Items.Clear;

  for i := 0 to Component.ComponentList.Count-1 do
  begin
    ListItem := lvSel.Items.Add;
    ListItem.Caption := Component.ComponentList[i];
    Comp := Component.Owner.FindComponent( Component.ComponentList[i] );
    if Comp = nil then continue;
    ColorPropInfo := GetPropInfo( Comp.ClassInfo, 'Color');
    FontPropInfo := GetPropInfo( Comp.ClassInfo, 'Font');
    ListItem.SubItems.Add(aSigns[Assigned(ColorPropInfo)]);
    ListItem.SubItems.Add(aSigns[Assigned(FontPropInfo)]);
    ListItem.ImageIndex := 1;
  end;

  with Component.Owner do
  for i:=0 to ComponentCount-1 do
  begin
    ColorPropInfo := GetPropInfo( Components[i].ClassInfo, 'Color');
    FontPropInfo := GetPropInfo( Components[i].ClassInfo, 'Font');
    if (ColorPropInfo <> nil)or(FontPropInfo <> nil) then
     begin
       ListItem := lvAll.Items.Add;
       ListItem.Caption := Components[i].Name;
       ListItem.SubItems.Add(aSigns[Assigned(ColorPropInfo)]);
       ListItem.SubItems.Add(aSigns[Assigned(FontPropInfo)]);

       for j:=0 to lvSel.Items.Count - 1 do
         if lvSel.Items[j].Caption = ListItem.Caption then
         begin ListItem.ImageIndex := 1; break; end;

     end;
    //  SetOrdProp( FormX.Components[i], PropInfo, clGreen );
  end;
end;

procedure TglCompListEditor.lvAllDblClick(Sender: TObject);
var ListItem: TListItem;
begin
  if (lvAll.Selected = nil)or(lvAll.Selected.ImageIndex = 1) then exit;

  ListItem := lvSel.Items.Add;
  ListItem.Caption := lvAll.Selected.Caption;
  ListItem.SubItems.Add(lvAll.Selected.SubItems[0]);
  ListItem.SubItems.Add(lvAll.Selected.SubItems[1]);
  lvAll.Selected.ImageIndex := 1;
  ListItem.ImageIndex := 1;
end;

procedure TglCompListEditor.BitBtn4Click(Sender: TObject);
begin close; end;

procedure TglCompListEditor.BitBtn3Click(Sender: TObject);
var
  i: integer;
  Comp: TComponent;  
begin
  Component.ComponentList.Clear;
  Component.CompList.Clear;
  for i := 0 to lvSel.Items.Count - 1 do
  begin
    Comp := Component.Owner.FindComponent( lvSel.Items[i].Caption );
    if Comp = nil then continue;
    Component.CompList.Add(Comp);
    Component.ComponentList.Add(lvSel.Items[i].Caption);
  end;
  close;
end;

procedure TglCompListEditor.lvSelDblClick(Sender: TObject);
var i: integer;
begin
  if not Assigned(lvSel.Selected) then exit;
  for i:=0 to lvAll.Items.Count - 1 do
    if lvAll.Items[i].Caption = lvSel.Selected.Caption then break;

  lvAll.Items[i].ImageIndex := 0;
  lvSel.Items.Delete(lvSel.Selected.Index);
end;

procedure TglCompListEditor.lvSelChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  pbRemove.Enabled := Assigned(lvSel.Selected);
end;

procedure TglCompListEditor.lvAllChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  pbAdd.Enabled := Assigned(lvAll.Selected) and (lvAll.Selected.ImageIndex = 0);
end;

end.
