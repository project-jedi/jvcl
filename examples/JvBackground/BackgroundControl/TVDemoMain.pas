unit TVDemoMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvBackgroundTreeview, ComCtrls, Menus, JvBackgrounds, StdCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    AppearanceMenu: TMenuItem;
    FontItem: TMenuItem;
    FontDialog: TFontDialog;
    ButtonStyleItem: TMenuItem;
    Rectangle1: TMenuItem;
    X1: TMenuItem;
    Circle1: TMenuItem;
    Splitter1: TSplitter;
    PropListView: TListView;
    Panel1: TPanel;
    ClassName: TLabel;
    InstanceSize: TLabel;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FontItemClick(Sender: TObject);
    procedure ChangeTVButtonKind(Sender: TObject);
    procedure TVChange(Sender: TObject; Node: TTreeNode);
    procedure FormShow(Sender: TObject);
  private
    TV: TJvBackgroundTreeView;
    procedure CreateTV;
    procedure InitTV;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

uses TypInfo;

procedure TForm1.InitTV;

  function ClassIndex(AClass: TClass): Integer;
  begin
    for Result := 0 to TV.Items.Count-1 do
      if TV.Items[Result].Text = AClass.ClassName then
        Exit;
    Result := -1;
  end;

  function AddHierarchy(AClass: TClass): TTreeNode;
  var
    I: Integer;
  begin
    if AClass = nil then
      Result := nil
    else
    begin
      I := ClassIndex(AClass);
      if I = -1 then
      begin
        Result := TV.Items.AddChild(AddHierarchy(AClass.ClassParent),
                          	    AClass.ClassName);
        Result.Data := AClass;
      end
      else Result := TV.Items[I];
    end
  end;

  procedure AddClasses(Component: TComponent);
  var
    I: Integer;
  begin
    AddHierarchy(Component.ClassType);
    for I := 0 to Component.ComponentCount-1 do
      AddClasses(Component.Components[I]);
  end;

begin
  TV.Items.BeginUpdate;
  try
    TV.Items.Clear;
    AddClasses(Self);
    TV.FullExpand;
  finally
    TV.Items.EndUpdate;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  CreateTV;
  InitTV;
end;

procedure TForm1.FontItemClick(Sender: TObject);
begin
  FontDialog.Font := TV.Font;
  if FontDialog.Execute then
    TV.Font := FontDialog.Font;
end;

procedure TForm1.ChangeTVButtonKind(Sender: TObject);
begin
  TV.ButtonKind := TJvBackgroundTVButtonKind(TComponent(Sender).Tag);
  (Sender as TMenuItem).Checked := True;
end;

type
  TPropInfos = array of PPropInfo;

function GetClassProperties(AClass: TClass; Kinds: TTypeKinds): TPropInfos;
var
  PropCount: Integer;
  TypeInfo: PTypeInfo;
begin
  TypeInfo := AClass.ClassInfo;
  if TypeInfo <> nil then
  begin
    PropCount := GetPropList(TypeInfo, Kinds, nil);
    if PropCount > 0 then
    begin
      SetLength(Result, PropCount);
      GetPropList(TypeInfo, Kinds, PPropList(Result));
    end;
  end;
end;

procedure TForm1.TVChange(Sender: TObject; Node: TTreeNode);
const
  AllKinds: TTypeKinds = [Low(TTypeKind)..High(TTypeKind)];
var
  I: Integer;
  Properties: TPropInfos;
  DefaultStr: string;
  AClass: TClass;
begin
  AClass := TClass(Node.Data);
  if AClass = TObject
    then ClassName.Caption := 'System.TObject'
    else ClassName.Caption := GetTypeData(AClass.ClassInfo).UnitName+'.'+AClass.ClassName;
  InstanceSize.Caption := IntToStr(AClass.InstanceSize);
  PropListView.Items.Clear;
  Properties := GetClassProperties(AClass, AllKinds);
  if Properties <> nil then
  try
    PropListView.Items.BeginUpdate;
    try
      for I := 0 to Length(Properties)-1 do
      with PropListView.Items.Add, Properties[I]^ do
      begin
        Caption := Name;
        SubItems.Add(PropType^^.Name);
        if Default = Low(Integer) then
          DefaultStr := '<none>'
        else
          if PropType^^.Kind = tkEnumeration
            then DefaultStr := GetEnumName(PropType^, Default)
            else if Abs(Default) > 10000
              then DefaultStr := Format('$%.8x', [Default])
              else DefaultStr := IntToStr(Default);
        SubItems.Add(DefaultStr);
      end;
    finally
      PropListView.Items.EndUpdate;
    end;
  finally
    Properties := nil;
  end;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  with PropListView.Columns[0] do Width := 120;
end;

procedure TForm1.CreateTV;
begin
  TV := TJvBackgroundTreeView.Create(Self);
  with TV do
  begin
    Parent := Self;
    Background.Picture.LoadFromFile(ExpandUNCFilename('Data\Marble.bmp'));
    Background.FitPictureSize := True;
    Background.GrayMapped := True;
    Background.TileWidth := 128;
    Background.TileHeight := 128;
    ButtonKind := tvbRectangle;
    ButtonSize := 4;
    SelectedColor := clRed;
    Align := alClient;
    Indent := 19;
    ParentColor := True;
    ParentFont := False;
    TabOrder := 0;
    OnChange := TVChange;
  end;
end;

end.
