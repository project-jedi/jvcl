unit DropFrm;
{$I JVCL.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ImgList;

type
  TDropFrmAcceptEvent = procedure (Sender:TObject; Index:integer; const Value:string) of object;
  TfrmDrop = class(TForm)
    Label1: TLabel;
    btnCancel: TButton;
    tvFolders: TTreeView;
    ImageList1: TImageList;
    btnOK: TButton;
    procedure tvFoldersDblClick(Sender: TObject);
    procedure tvFoldersExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure tvFoldersGetImageIndex(Sender: TObject; Node: TTreeNode);
    procedure tvFoldersGetSelectedIndex(Sender: TObject; Node: TTreeNode);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FOnAccept: TDropFrmAcceptEvent;
    procedure BuildFolderList(Items: TTreeNodes; Parent: TTreeNode; const Root: string; Level: integer);
    procedure BuildFileSystem;

  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WMActivate(var Message: TWMActivate); message WM_ACTIVATE;
    { Private declarations }
  public
    { Public declarations }
    property OnAccept:TDropFrmAcceptEvent read FOnAccept write FOnAccept;

  end;

var
  frmDrop: TfrmDrop = nil;

function GetFullPath(Item: TTreeNode): string;

implementation
{$IFNDEF COMPILER6_UP}
uses
 JvJCLUtils, // DirectoryExists
 JvJVCLUtils; // Include/ExcludeTrailingPathDelimiter
{$ENDIF} 

{$R *.dfm}

function GetFullPath(Item: TTreeNode): string;
begin
  Result := '';
  while Item <> nil do
  begin
    Result := Item.Text + '\' + Result;
    Item := Item.Parent;
  end;
  Result := IncludeTrailingPathDelimiter(ExtractFileDrive(Application.Exename)) + Result;
end;

{ TfrmDrop }

procedure TfrmDrop.BuildFileSystem;
var
  S: string;
begin
  tvFolders.Items.BeginUpdate;
  Screen.Cursor := crHourGlass;
  try
    tvFolders.Items.Clear;
    S := ExtractFileDrive(Application.ExeName);
    BuildFolderList(tvFolders.Items, nil, S, 2);
    // tvFolders.Items.AddChild(nil,S)
  finally
    tvFolders.Items.EndUpdate;
    Screen.Cursor := crDefault;
  end;
//  tvFolders.Items.GetFirstNode.Expand(false);
end;

procedure TfrmDrop.BuildFolderList(Items: TTreeNodes; Parent: TTreeNode; const Root: string; Level: integer);
var
  F: TSearchRec;
  S: string;
begin
  if Level = 0 then Exit;
  S := IncludeTrailingPathDelimiter(Root);
  if FindFirst(S + '*.*', faDirectory, F) = 0 then
  begin
    repeat
      if (F.Name[1] <> '.') and DirectoryExists(S + F.Name) then
        BuildFolderList(Items, Items.AddChild(Parent, F.Name), S + F.Name, Level - 1);
    until FindNext(F) <> 0;
    FindClose(F);
  end;
end;

procedure TfrmDrop.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.Style := Params.Style and not WS_BORDER;
end;

procedure TfrmDrop.tvFoldersDblClick(Sender: TObject);
begin
  if (tvFolders.Selected <> nil) and (not tvFolders.Selected.HasChildren) then
    btnOK.Click;
end;

procedure TfrmDrop.tvFoldersExpanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
begin
  Node.DeleteChildren;
  Screen.Cursor := crHourGlass;
  tvFolders.Items.BeginUpdate;
  try
    BuildFolderList(tvFolders.Items, Node, GetFullPath(Node), 2);
  finally
    Screen.Cursor := crDefault;
    tvFolders.Items.EndUpdate;
  end;
end;

procedure TfrmDrop.WMActivate(var Message: TWMActivate);
begin
  inherited;
  if (Message.Active = WA_INACTIVE) then
    btnCancel.Click;
end;

procedure TfrmDrop.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if (ModalResult = mrOK) and Assigned(FOnAccept) then
    FOnAccept(self, -1, ExcludeTrailingPathDelimiter(GetFullPath(tvFolders.Selected)));
//  Action := caFree;
//  frmDrop := nil;
end;                            

procedure TfrmDrop.btnCancelClick(Sender: TObject);
begin
  if not (fsModal in FormState) then
    Close;
end;

procedure TfrmDrop.btnOKClick(Sender: TObject);
begin
  if not (fsModal in FormState) then
    Close;
end;

procedure TfrmDrop.tvFoldersGetImageIndex(Sender: TObject;
  Node: TTreeNode);
begin
  if Node.Expanded or Node.Selected then
    Node.ImageIndex := 1
  else
    Node.ImageIndex := 0;
end;

procedure TfrmDrop.tvFoldersGetSelectedIndex(Sender: TObject;
  Node: TTreeNode);
begin
  if Node.Expanded or Node.Selected then
    Node.SelectedIndex := 1
  else
    Node.SelectedIndex := 0;
end;

procedure TfrmDrop.FormCreate(Sender: TObject);
begin
  BuildFileSystem;
end;

procedure TfrmDrop.FormShow(Sender: TObject);
begin
  if tvFolders.CanFocus then tvFolders.SetFocus;
end;

end.

