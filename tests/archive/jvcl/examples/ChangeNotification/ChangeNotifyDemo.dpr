program ChangeNotifyDemo;

uses
  Forms, ChangeNotificationMainFormU, ChangeNotificationDirDlgU;

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'TChangeNotification Demo';
  Application.CreateForm(TChangeNotificationMainForm, ChangeNotificationMainForm);
  Application.Run;
end.
