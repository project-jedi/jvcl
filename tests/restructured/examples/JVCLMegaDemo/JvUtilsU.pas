unit JvUtilsU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvApplication, JvComponent, StdCtrls, JvDirectories, JvChrono, JvKeyboardStates,
  MPlayer, JvFunctions, JvButton, JvRecentMenuBtn, JvStartMenuBtn, JvScreenSaver,
  JvControlPanel, JvFavoritesButton;

type
  TJvUtilsFrm = class(TFrame)
    JvDirectories1: TJvDirectories;
    Memo1: TMemo;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button5: TButton;
    JvKeyboardStates1: TJvKeyboardStates;
    Button6: TButton;
    Button7: TButton;
    Button10: TButton;
    CheckBox2: TCheckBox;
    JvRecentMenuBtn1: TJvRecentMenuBtn;
    JvStartMenuBtn1: TJvStartMenuBtn;
    JvFavoritesButton1: TJvFavoritesButton;
    JvControlPanel1: TJvControlPanel;
    CheckBox1: TCheckBox;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

implementation

{$R *.DFM}

procedure TJvUtilsFrm.Button1Click(Sender: TObject);
begin
 with Memo1.Lines do
  begin
   Add('CurrentDirectory: ' + JvDirectories1.CurrentDirectory);
   Add('WindowsDirectory: ' + JvDirectories1.WindowsDirectory);
   Add('SystemDirectory:: ' + JvDirectories1.SystemDirectory);
   Add('TempPath: ' + JvDirectories1.TempPath);
   Add('ApplicationData: ' + JvDirectories1.ApplicationData);
   Add('Cache: ' + JvDirectories1.Cache);
   Add('Cookies: ' + JvDirectories1.Cookies);
   Add('Desktop: ' + JvDirectories1.Desktop);
   Add('Favorites: ' + JvDirectories1.Favorites);
   Add('Fonts: ' + JvDirectories1.Fonts);
   Add('History: ' + JvDirectories1.History);
   Add('NetHood: ' + JvDirectories1.NetHood);
   Add('Personal: ' + JvDirectories1.Personal);
   Add('Programs: ' + JvDirectories1.Programs);
   Add('ProgramFiles: ' + JvDirectories1.ProgramFiles);
   Add('Recent: ' + JvDirectories1.Recent);
   Add('SendTo: ' + JvDirectories1.SendTo);
   Add('StartMenu: ' + JvDirectories1.StartMenu);
   Add('Startup: ' + JvDirectories1.Startup);
   Add('Templates: ' + JvDirectories1.Templates);
 end;
end;

procedure TJvUtilsFrm.Button2Click(Sender: TObject);
begin
 MonitorOff();  
end;

procedure TJvUtilsFrm.Button3Click(Sender: TObject);
begin
 AddToRecentDocs(Application.ExeName);
 ShowMessage('Done');
end;

procedure TJvUtilsFrm.Button5Click(Sender: TObject);
begin
 LaunchCPL(JvDirectories1.SystemDirectory + 'DESK.CPL');
end;


procedure TJvUtilsFrm.Button6Click(Sender: TObject);
begin
 case JvKeyboardStates1.tag of
  0: begin
       JvKeyboardStates1.Enabled := true;
       JvKeyboardStates1.tag := 1;
       Button6.Caption := 'Stop that!!';
     end;

  1: begin
       JvKeyboardStates1.Enabled := false;
       JvKeyboardStates1.tag := 0;
       Button6.Caption := 'let me see this again because it was so beautiful';
    end;
  end; // of case
end;

procedure TJvUtilsFrm.Button7Click(Sender: TObject);
begin
 if JvKeyboardStates1.NumLock then Memo1.Lines.Add('NumLock is on')
 else Memo1.Lines.Add('NumLock is off');

 if JvKeyboardStates1.ScrollLock then Memo1.Lines.Add('ScrollLock is on')
 else Memo1.Lines.Add('ScrollLock is off');

 if JvKeyboardStates1.CapsLock then Memo1.Lines.Add('CapsLock is on')
 else Memo1.Lines.Add('CapsLock is off');
end;

procedure TJvUtilsFrm.CheckBox2Click(Sender: TObject);
begin
 if CheckBox2.Checked then HideTraybar
 else ShowTraybar;
end;

procedure TJvUtilsFrm.Button10Click(Sender: TObject);
begin
 if DiskInDrive('A') = true then ShowMessage('It is!')
 else ShowMessage('There is no disk in drive A')
end;

procedure TJvUtilsFrm.CheckBox1Click(Sender: TObject);
begin
 if CheckBox1.Checked then HideStartButton
 else ShowStartButton;
end;

end.
