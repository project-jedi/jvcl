unit FindAndReplaceUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ToolWin, StdCtrls, JvDockControlForm;

type
  TFindAndReplaceForm = class(TForm)
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    Find: TButton;
    ReplaceButton: TButton;
    ReplaceAllButton: TButton;
    FindComboBox: TComboBox;
    ReplaceComboBox: TComboBox;
    LookInLabel: TLabel;
    LookInComboBox: TComboBox;
    SubfoldersCheckBox: TCheckBox;
    BrowseButton: TButton;
    ListView: TListView;
    lbDockClient1: TJvDockClient;
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FLookInlabelRect,
    FLookInComboboxRect,
    FSubfoldersCheckBoxRect,
    FBrowseButtonRect,
    FListViewRect: TRect;
  public
    { Public declarations }
  end;

var
  FindAndReplaceForm: TFindAndReplaceForm;

implementation

uses MainFormUnit;

{$R *.DFM}

procedure TFindAndReplaceForm.FormResize(Sender: TObject);
var LookInMoveDown: Boolean;
    BrowseMoveDown: Boolean;
begin
  BrowseMoveDown := ClientWidth - (SubfoldersCheckBox.Left + SubfoldersCheckBox.Width) < BrowseButton.Width + 5;
  LookInMoveDown := ClientWidth - (ReplaceAllButton.Left + ReplaceAllButton.Width) < 220;
  if LookInMoveDown then
  begin
    LookInLabel.Left := 5;
    LookInComboBox.Left := LookInLabel.Left + LookInLabel.Width;
    LookInLabel.Top := FLookInlabelRect.Top + 27;
    LookInComboBox.Top := LookInLabel.Top - 3;
    SubfoldersCheckBox.Left := 5;
    SubfoldersCheckBox.Top := FSubfoldersCheckBoxRect.Top + 27;
  end else
  begin
    LookInLabel.Left := FLookInlabelRect.Left;
    LookInComboBox.Left := LookInLabel.Left + LookInLabel.Width;
    LookInLabel.Top := FLookInlabelRect.Top;
    LookInComboBox.Top := LookInLabel.Top - 3;
    SubfoldersCheckBox.Left := FSubfoldersCheckBoxRect.Left;
    SubfoldersCheckBox.Top := FSubfoldersCheckBoxRect.Top;
  end;

  if BrowseMoveDown then
  begin
    BrowseButton.Left := SubfoldersCheckBox.Left;
    BrowseButton.Top := SubfoldersCheckBox.Top + 22;
  end else
  begin
    BrowseButton.Left := ClientWidth - BrowseButton.Width - 5;
    BrowseButton.Top := SubfoldersCheckBox.Top;
  end;

  LookInComboBox.Width := ClientWidth - (LookInLabel.Left + LookInLabel.Width) - 5;
  FindComboBox.Width := ClientWidth - FindComboBox.Left - 5;
  ReplaceComboBox.Width := ClientWidth - ReplaceComboBox.Left - 5;
  ListView.Top := FListViewRect.Top + (Integer(LookInMoveDown) + Integer(BrowseMoveDown)) * 26;
  ListView.Width := ClientWidth - ListView.Left - 5;
  ListView.Height := ClientHeight - ListView.Top - 5;
end;

procedure TFindAndReplaceForm.FormCreate(Sender: TObject);
begin
  FLookInlabelRect := LookInLabel.BoundsRect;
  FLookInComboboxRect := LookInCombobox.BoundsRect;
  FSubfoldersCheckBoxRect := SubfoldersCheckBox.BoundsRect;
  FBrowseButtonRect := BrowseButton.BoundsRect;
  FListViewRect := listView.BoundsRect;
end;

end.
