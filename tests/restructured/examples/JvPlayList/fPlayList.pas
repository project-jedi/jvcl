unit fPlayList;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ActnList, Menus, JvPlaylist, JvListBox, JvCtrls;

type
  TForm1 = class(TForm)
    JvPlaylist1: TJvPlaylist;
    OpenDialog1: TOpenDialog;
    Label1: TLabel;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Open1: TMenuItem;
    Delete1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    ActionList1: TActionList;
    Open: TAction;
    Delete: TAction;
    Exit: TAction;
    Label2: TLabel;
    Options1: TMenuItem;
    ShowNumbers1: TMenuItem;
    ShowExtensions1: TMenuItem;
    Operations1: TMenuItem;
    DeleteDead: TAction;
    DeleteDeadFiles1: TMenuItem;
    Delete2: TMenuItem;
    SortSong: TAction;
    SortPah: TAction;
    SortPathI: TAction;
    SortSongNameInverted: TAction;
    RandomOrder: TAction;
    Reverse: TAction;
    SortByPath1: TMenuItem;
    SortByPathInverted1: TMenuItem;
    SortBySongName1: TMenuItem;
    SortBySongNameInverted1: TMenuItem;
    N2: TMenuItem;
    RandomOrder1: TMenuItem;
    ReverseOrder1: TMenuItem;
    Selection1: TMenuItem;
    SelectAll: TAction;
    UnselectAll: TAction;
    InvSelect: TAction;
    SelectAll1: TMenuItem;
    UnselectAll1: TMenuItem;
    InverseSelection1: TMenuItem;
    N3: TMenuItem;
    MoveUp: TAction;
    MoveDown: TAction;
    MoveSelectedUp1: TMenuItem;
    MoveSelectedDown1: TMenuItem;
    procedure JvPlaylist1Click(Sender: TObject);
    procedure OpenExecute(Sender: TObject);
    procedure ExitExecute(Sender: TObject);
    procedure DeleteExecute(Sender: TObject);
    procedure Options1Click(Sender: TObject);
    procedure ShowNumbers1Click(Sender: TObject);
    procedure ShowExtensions1Click(Sender: TObject);
    procedure SortSongExecute(Sender: TObject);
    procedure SortPahExecute(Sender: TObject);
    procedure SortPathIExecute(Sender: TObject);
    procedure SortSongNameInvertedExecute(Sender: TObject);
    procedure RandomOrderExecute(Sender: TObject);
    procedure ReverseExecute(Sender: TObject);
    procedure DeleteDeadExecute(Sender: TObject);
    procedure SelectAllExecute(Sender: TObject);
    procedure UnselectAllExecute(Sender: TObject);
    procedure InvSelectExecute(Sender: TObject);
    procedure MoveUpExecute(Sender: TObject);
    procedure MoveDownExecute(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}


procedure TForm1.JvPlaylist1Click(Sender: TObject);
begin
  if JvPlayList1.ItemIndex<>-1 then
    Label1.Caption := JvPlayList1.Items[JvPlayList1.ItemIndex];
end;

procedure TForm1.OpenExecute(Sender: TObject);
begin
  if OpenDialog1.Execute then
    JvPlayList1.AddItems(OpenDialog1.Files);
end;

procedure TForm1.ExitExecute(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TForm1.DeleteExecute(Sender: TObject);
begin
  JvPlayList1.DeleteSelected;
end;

procedure TForm1.Options1Click(Sender: TObject);
begin
  ShowNumbers1.Checked := JvPlayList1.ShowNumbers;
  ShowExtensions1.Checked := JvPlayList1.ShowExtension;
end;

procedure TForm1.ShowNumbers1Click(Sender: TObject);
begin
  with Sender as TMenuItem do
  begin
    Checked := not Checked;
    JvPlayList1.ShowNumbers := Checked;
  end;
end;

procedure TForm1.ShowExtensions1Click(Sender: TObject);
begin
  with Sender as TMenuItem do
  begin
    Checked := not Checked;
    JvPlayList1.ShowExtension := Checked;
  end;
end;

procedure TForm1.SortSongExecute(Sender: TObject);
begin
  JvPlayList1.SortBySongName;
end;

procedure TForm1.SortPahExecute(Sender: TObject);
begin
  JvPlayList1.SortByPath;
end;

procedure TForm1.SortPathIExecute(Sender: TObject);
begin
  JvPlayList1.SortByPathInverted;
end;

procedure TForm1.SortSongNameInvertedExecute(Sender: TObject);
begin
  JvPlayList1.SortBySongNameInverted;
end;

procedure TForm1.RandomOrderExecute(Sender: TObject);
begin
  JvPlayList1.RandomOrder;
end;

procedure TForm1.ReverseExecute(Sender: TObject);
begin
  JvPlayList1.ReverseOrder;
end;

procedure TForm1.DeleteDeadExecute(Sender: TObject);
begin
  JvPlayList1.DeleteDeadFiles;
end;

procedure TForm1.SelectAllExecute(Sender: TObject);
begin
  JvPlayList1.SelectAll;
end;

procedure TForm1.UnselectAllExecute(Sender: TObject);
begin
  JvPlayList1.UnselectAll;
end;

procedure TForm1.InvSelectExecute(Sender: TObject);
begin
  JvPlayList1.InvertSelection;
end;

procedure TForm1.MoveUpExecute(Sender: TObject);
begin
  JvPlayList1.MoveSelectedUp;
end;

procedure TForm1.MoveDownExecute(Sender: TObject);
begin
  JvPlayList1.MoveSelectedDown;
end;

end.
