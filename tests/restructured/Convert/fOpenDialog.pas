unit fOpenDialog;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  JvToolEdit,
  StdCtrls,
  Mask,
  ComCtrls,
  ShellCtrls,
  ExtCtrls,
  JvSearchFiles;

type
  TfrmOpenFiles = class(TForm)
    ShellTreeView1: TShellTreeView;
    Panel1: TPanel;
    cbxFilesTypes: TComboBox;
    Label1: TLabel;
    btnOK: TButton;
    btnCancel: TButton;
    lblDirectory: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ShellTreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure ShellTreeView1GetSelectedIndex(Sender: TObject;
      Node: TTreeNode);
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
    { Private declarations }
    fJvSearchFiles:TJvSearchFiles;
    function GetDirStr(node: TTreeNode): string;
  public
    { Public declarations }
    constructor CreateWithDirString (const AOwner:TComponent; var JvSearchFiles1:TJvSearchFiles);
  end;

var
  frmOpenFiles: TfrmOpenFiles;

implementation

{$R *.dfm}

procedure TfrmOpenFiles.FormCreate(Sender: TObject);
begin
  cbxFilesTypes.ItemIndex:=0;
end;

function TfrmOpenFiles.GetDirStr(node: TTreeNode): string;
var
  DirString, Root: string;
  CurrentNode: TTreeNode;
  SubPos: Integer;
begin
  result := '';
  if node = nil then exit;
  DirString := Node.Text;
  CurrentNode := Node;
  while CurrentNode.Parent <> nil do
    begin
      CurrentNode := CurrentNode.Parent;
      DirString := CurrentNode.Text + '\' + DirString;
    end;

  subPos := pos(':', DirString);
  if subPos > 0 then
    begin
      Root := copy(DirString, SubPos - 1, 2);
      subPos := pos(')', DirString);
      if SubPos > 0 then
        DirString := Copy(DirString, SubPos + 1, length(DirString) - SubPos);
      DirString := Root + DirString;
    end;

  Result := DirString + '\';

end;

procedure TfrmOpenFiles.ShellTreeView1Change(Sender: TObject;
  Node: TTreeNode);
begin
  lblDirectory.Caption := GetDirStr(node); //Node.GetNamePath;
end;

procedure TfrmOpenFiles.ShellTreeView1GetSelectedIndex(Sender: TObject;
  Node: TTreeNode);
begin
  lblDirectory.Caption := Node.GetNamePath;
end;

procedure TfrmOpenFiles.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;

end;

procedure TfrmOpenFiles.btnOKClick(Sender: TObject);
begin
fJvSearchFiles.FilePath:= lblDirectory.Caption;


fJvSearchFiles.FileMask:=cbxFilesTypes.Text;
ModalResult := mrOK;
end;

constructor TfrmOpenFiles.CreateWithDirString(const AOwner: TComponent; var JvSearchFiles1:TJvSearchFiles);
begin
fJvSearchFiles:=JvSearchFiles1;
inherited Create(AOwner);
end;

end.

