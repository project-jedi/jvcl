unit fInstall;

{
  This is a simple installer showing you how to use those
  components. Those components are just interfaces for the installer,
  Jvt they doesn't decompress/create links and other stuff's like that !.

  For creating links, use the  TJvCreateShortcut in the Jv Utils palette,
  and to decompress something, use the TJvCabFile component  in the Jv Utils2
  palette.

  This sample doesn't modify anything on your system ! You can run it
  without any problem, the installation progress is simply a fake
  random loop.

  To enable uninstall features, look at the end of the code, I explain
  you how to use my components to do such a thing, and of course, you'll
  have to create an uninstaller using the TJvUninstaller component
}


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvWelcome, JvInstaller, JvGradient, JvAgreement, JvInstallerPage,
  JvConfirmCancel, JvReadme, JvSelectDir, JvSelectGroup, JvInstallType,
  JvCompoInstall, JvInstallProgress, JvUsernameSerial, JvFinishInstall,
  JvInstallConfirm, JvComponent;

type
  TForm1 = class(TForm)
    JvGradient1: TJvGradient;
    JvInstaller1: TJvInstaller;
    JvWelcome1: TJvWelcome;
    JvUsernameSerial1: TJvUsernameSerial;
    JvInstallProgress1: TJvInstallProgress;
    JvCompoInstall1: TJvCompoInstall;
    JvInstallType1: TJvInstallType;
    JvSelectGroup1: TJvSelectGroup;
    JvSelectDir1: TJvSelectDir;
    JvReadme1: TJvReadme;
    JvAgreement1: TJvAgreement;
    JvFinishInstall1: TJvFinishInstall;
    JvConfirmCancel1: TJvConfirmCancel;
    JvInstallConfirm1: TJvInstallConfirm;
    procedure JvInstaller1Loaded(Sender: TObject);
    procedure JvInstaller1Cancel(Sender: TObject);
    procedure JvConfirmCancel1Exit(Sender: TObject);
    procedure JvInstallProgress1NextPageShown(Sender: TObject);
    procedure JvUsernameSerial1NextPage(Sender: TObject;
      var Accept: Boolean);
    procedure JvFinishInstall1Next(Sender: TObject);
  private
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.JvInstaller1Loaded(Sender: TObject);
begin
  JvWelcome1.Execute;
end;

procedure TForm1.JvInstaller1Cancel(Sender: TObject);
begin
  JvConfirmCancel1.Execute;
end;

procedure TForm1.JvConfirmCancel1Exit(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TForm1.JvInstallProgress1NextPageShown(Sender: TObject);
var
 i,j:integer;
begin
  //Here, you have to decompress and install your files
  //You can extract your files from resources, and eventually
  //put them in a CAB to compress them
  //(If you use a CAB, you'll have to use the TJvCabFile to decompress it
  //You can also use the TZLibStream in the ZLib unit (on your delphi cdrom).

  JvInstallProgress1.Options.Total.Maximum:=30;
  for i:=1 to 30 do
  begin
    JvInstallProgress1.Options.Total.Progress:=i;
    JvInstallProgress1.Options.Current.Maximum:=40;
    for j:=1 to 40 do
    begin
      JvInstallProgress1.Options.FileName:='Filename xxx.'+IntToStr(j);
      JvInstallProgress1.Options.Current.Progress:=j;
      sleep(random(30));
    end;
  end;
  JvInstallProgress1.Terminate;
end;

procedure TForm1.JvUsernameSerial1NextPage(Sender: TObject;
  var Accept: Boolean);
begin
  if JvUsernameSerial1.Options.Serial=JvUsernameSerial1.Options.Username then
    Accept:=true
  else
  begin
    Accept:=false;
    ShowMessage('The username and the serial must be the same to continue ;p');
  end;
end;

procedure TForm1.JvFinishInstall1Next(Sender: TObject);
begin
  //installation finished

  //You have to check for what to do (following checkboxes in the JvFinishInstall1)

  //To add Uninstall possibility, just use this following line
  //JvInstaller1.AddUninstall('The title of your application','c:\the directory\your uninstaller.exe');

  Application.Terminate;
end;

end.
