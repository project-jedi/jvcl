{$I JVCL.INC}
unit Main;

interface

uses
  Windows, Messages, SysUtils, {$IFDEF DELPHI6_UP}Variants, {$ENDIF}Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, JvRegTV, ExtCtrls, Menus{, JvVersionInfo};

type
  TForm1 = class(TForm)
    StatusBar1: TStatusBar;
    RegistryTreeView1: TJvRegistryTreeView;
    ListView1: TListView;
    Splitter1: TSplitter;
    mmMain: TMainMenu;
    Registry1: TMenuItem;
    Importregistryfile1: TMenuItem;
    Exportregistryfile1: TMenuItem;
    N1: TMenuItem;
    Connectnetworkdrive1: TMenuItem;
    Disconnectnetworkdrive1: TMenuItem;
    N2: TMenuItem;
    Print1: TMenuItem;
    N3: TMenuItem;
    Exit1: TMenuItem;
    Edit1: TMenuItem;
    New1: TMenuItem;
    Key1: TMenuItem;
    N4: TMenuItem;
    Stringvalue1: TMenuItem;
    Binaryvalue1: TMenuItem;
    DWORDvalue1: TMenuItem;
    N5: TMenuItem;
    Delete1: TMenuItem;
    Rename1: TMenuItem;
    N6: TMenuItem;
    Copykeyname1: TMenuItem;
    N7: TMenuItem;
    Find1: TMenuItem;
    FindNext1: TMenuItem;
    View1: TMenuItem;
    Statusbar2: TMenuItem;
    N8: TMenuItem;
    Refresh1: TMenuItem;
    Favorites1: TMenuItem;
    Addtofavorites1: TMenuItem;
    Deletefavorite1: TMenuItem;
    N10: TMenuItem;
    Help1: TMenuItem;
    HelpIndex1: TMenuItem;
    N11: TMenuItem;
    AboutRegistryeditordemo1: TMenuItem;
    procedure RegistryTreeView1Expanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure RegistryTreeView1Expanded(Sender: TObject; Node: TTreeNode);
    procedure RegistryTreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure Refresh1Click(Sender: TObject);
    procedure Statusbar2Click(Sender: TObject);
    procedure Delete1Click(Sender: TObject);
    procedure AboutRegistryeditordemo1Click(Sender: TObject);
    procedure Rename1Click(Sender: TObject);
    procedure Key1Click(Sender: TObject);
    procedure Addtofavorites1Click(Sender: TObject);
    procedure Importregistryfile1Click(Sender: TObject);
    procedure Exportregistryfile1Click(Sender: TObject);
    procedure Connectnetworkdrive1Click(Sender: TObject);
    procedure Disconnectnetworkdrive1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
  private
    procedure DoFavoriteClick(Sender: TObject);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
uses
  Registry;

{$R *.dfm}

procedure TForm1.RegistryTreeView1Expanding(Sender: TObject;
  Node: TTreeNode; var AllowExpansion: Boolean);
begin
  Screen.Cursor := crHourGlass;
end;

procedure TForm1.RegistryTreeView1Expanded(Sender: TObject;
  Node: TTreeNode);
begin
  Screen.Cursor := crDefault;
end;

procedure TForm1.RegistryTreeView1Change(Sender: TObject; Node: TTreeNode);
begin
  StatusBar1.Panels[0].Text := RegistryTreeView1.CurrentPath;
end;

procedure TForm1.Refresh1Click(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
    RegistryTreeView1.RefreshNode(RegistryTreeView1.Selected);
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TForm1.Statusbar2Click(Sender: TObject);
begin
  StatusBar1.Visible := Statusbar2.Checked;
end;

procedure TForm1.Delete1Click(Sender: TObject);
begin
  if RegistryTreeView1.Selected <> nil then
    RegistryTreeView1.Selected.Delete;
end;

procedure TForm1.AboutRegistryeditordemo1Click(Sender: TObject);
var S,T,U:string;
begin
{  with TVersionInfo.Create(Application.Exename) do
  try
    S := FileVersion;
    T := DateTimeToStr(FileDate);
  finally
    Free;
  end;
  ShowMessageFmt('Demo of the TRegistryTreeview component.'#13#10+
                 'Version: %s, Compile date: %s'#13#10#13#10+
                 'NOTE: edits are not saved to the registry.',
    [S,T]);}
  ShowMessage('Demo of the TRegistryTreeview component.'#13#10+
                 'NOTE: edits are not saved to the registry.');
end;

procedure TForm1.Rename1Click(Sender: TObject);
begin
  if RegistryTreeView1.Selected <> nil then
    RegistryTreeView1.Selected.EditText;
end;

procedure TForm1.Key1Click(Sender: TObject);
begin
  RegistryTreeView1.Items.AddChild(RegistryTreeView1.Selected,'New key').EditText;
end;

procedure TForm1.DoFavoriteClick(Sender:TObject);
var N:TTreeNode;
begin
  N := TTreeNode((Sender as TMenuItem).Tag);
  if N <> nil then
  begin
    N.Selected := true;
    N.Focused := true;
    N.MakeVisible;
  end;
end;

procedure TForm1.Addtofavorites1Click(Sender: TObject);
var S:string;m:TMenuItem;
begin
  if RegistryTreeView1.Selected <> nil then
  begin
    S := RegistryTreeView1.CurrentPath;
    if InputQuery('Add to favorites','Name:',S) and (S <> '') then
    begin
      m := TMenuItem.Create(mmMain);
      m.Caption := S;
      m.Tag     := integer(RegistryTreeView1.Selected);
      m.OnClick := DoFavoriteClick;
      m.AutoHotkeys := maManual;
      Favorites1.Add(m);
    end;
  end;
end;

procedure TForm1.Importregistryfile1Click(Sender: TObject);
begin
  with TOpenDialog.Create(nil) do
  try
    Filter := 'Registry files|*.reg;*.key|All files|*.*';
    InitialDir := '.';
    if Execute then
      RegistryTreeView1.LoadKey(Filename);
  finally
    Free;
  end;
end;

procedure TForm1.Exportregistryfile1Click(Sender: TObject);
begin
  with TSaveDialog.Create(nil) do
  try
    Filter := 'Registry files|*.reg;*.key|All files|*.*';
    InitialDir := '.';
    if Execute then
      RegistryTreeView1.SaveKey(Filename);
  finally
    Free;
  end;
end;

procedure TForm1.Connectnetworkdrive1Click(Sender: TObject);
begin
  //  RegConnectRegistry();
end;

procedure TForm1.Disconnectnetworkdrive1Click(Sender: TObject);
begin
  // RegCloseKey(RemoteRegKey);
end;

procedure TForm1.Exit1Click(Sender: TObject);
begin
  Close;
end;

end.
