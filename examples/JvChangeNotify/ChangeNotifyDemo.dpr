program ChangeNotifyDemo;

uses
  Forms,
  ChangeNotificationMainFormU in 'ChangeNotificationMainFormU.pas' {ChangeNotificationMainForm},
  ChangeNotificationDirDlgU in 'ChangeNotificationDirDlgU.pas' {ChangeNotificationDirDlg};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'TChangeNotification Demo';
  Application.CreateForm(TChangeNotificationMainForm, ChangeNotificationMainForm);
  Application.Run;
end.
