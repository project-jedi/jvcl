unit PlugInDemoU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvPluginMan, StdCtrls, Buttons, Menus, ExtCtrls, ImgList, JvComponent;

type
  TForm1 = class(TForm)
    uilPluginManager: TJvPluginManager;
    Listbox1: TListBox;
    MainMenu1: TMainMenu;
    Plugin1: TMenuItem;
    Panel1: TPanel;
    File1: TMenuItem;
    Exit1: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    Label1: TLabel;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure uilPluginManagerNewCommand(Sender: TObject; ACaption, AHint,
      AData: String; ABitmap: TBitmap; AEvent: TNotifyEvent);
    procedure SendMessagetoPlugins1Click(Sender: TObject);
  private
    { Private declarations }
  public
    NumButtons : integer;
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
begin
   uilPluginManager.LoadPlugins;
end;

procedure TForm1.Exit1Click(Sender: TObject);
begin
   Close;
end;

procedure TForm1.About1Click(Sender: TObject);
begin
   ShowMessage('A simple host application for demoing JEDI plug-ins.'#13#10#13#10'(c) 1999, Unlimited Intelligence Limited.');
end;

procedure TForm1.uilPluginManagerNewCommand(Sender: TObject; ACaption,
  AHint, AData: String; ABitmap: TBitmap; AEvent: TNotifyEvent);
var
   Item : TMenuItem;
begin
   Item := NewItem(ACaption, scNone, False, True, AEvent, 0, '');
   MainMenu1.Items[1].Add(Item);
   with TSpeedButton.Create(Panel1) do
   begin
      Top := 4;
      Left := 4+(NumButtons * Width);
      Parent := Panel1;
      Hint := AHint;
      if ABitmap <> nil then
         Glyph.Handle := ABitmap.Handle;
      try
         NumGlyphs := Glyph.Width div Glyph.Height;
      except
      end;
      OnClick := AEvent;
   end;    // with
   Inc(NumButtons);

end;

procedure TForm1.SendMessagetoPlugins1Click(Sender: TObject);
begin
   uilPluginManager.SendMessage(1000, InputBox('Enter message to send to plugin', 'Message', 'Your message here'));
end;

end.
