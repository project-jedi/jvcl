unit FileListBoxMainFormU;

interface

uses 
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  FileCtrl, JvDriveCtrls, StdCtrls, JvCombobox, JvListBox, JvLabel,
  JvComponent, JvFileInfo, ExtCtrls, JvCaptionPanel;

type
  TFileListBoxMainForm = class(TForm)
    JvLabel6: TJvLabel;
    JvLabel8: TJvLabel;
    JvDriveList1: TJvDriveList;
    Label1: TLabel;
    Label2: TLabel;
    JvCaptionPanel1: TJvCaptionPanel;
    JvFileListBox1: TJvFileListBox;
    JvDriveCombo1: TJvDriveCombo;
    JvDirectoryListBox1: TJvDirectoryListBox;
    Edit1: TEdit;
    Label3: TLabel;
  end;

var
  FileListBoxMainForm : TFileListBoxMainForm;

implementation

{$R *.DFM}

end.
