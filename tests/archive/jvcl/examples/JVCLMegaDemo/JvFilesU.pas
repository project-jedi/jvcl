unit JvFilesU;

interface

uses 
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  FileCtrl, JvDriveCtrls, StdCtrls, JvCombobox, JvListBox, JvLabel,
  JvComponent, JvFileInfo, ExtCtrls, JvCaptionPanel;

type
  TJvFilesFrm = class(TFrame)
    JvLabel6: TJvLabel;
    JvLabel8: TJvLabel;
    JvDriveList1: TJvDriveList;
    Label1: TLabel;
    Label2: TLabel;
    JvCaptionPanel1: TJvCaptionPanel;
    JvFileListBox1: TJvFileListBox;
    JvDriveCombo1: TJvDriveCombo;
    JvDirectoryListBox1: TJvDirectoryListBox;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.DFM}

end.
