unit MDISampleU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Menus, ComCtrls, Buttons, JvPluginMan, JvComponent;

type
  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Window1: TMenuItem;
    Exit1: TMenuItem;
    panToolbar: TPanel;
    uilPluginManager1: TJvPluginManager;
    StatusBar1: TStatusBar;
    Commands1: TMenuItem;
    procedure Exit1Click(Sender: TObject);
    procedure uilPluginManager1AfterLoading(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure uilPluginManager1NewCommand(Sender: TObject; ACaption, AHint,
      AData: String; ABitmap: TBitmap; AEvent: TNotifyEvent);
  private
    { Private declarations }
       NumButtons : integer;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Exit1Click(Sender: TObject);
begin
   Close;
end;

procedure TForm1.uilPluginManager1AfterLoading(Sender: TObject);
begin
   Statusbar1.SimpleText := 'Plugins Loaded: ' + IntToStr(uilPluginManager1.PluginCount);
end;

procedure TForm1.FormShow(Sender: TObject);
begin
   uilPluginManager1.LoadPlugins;
end;

procedure TForm1.uilPluginManager1NewCommand(Sender: TObject; ACaption,
  AHint, AData: String; ABitmap: TBitmap; AEvent: TNotifyEvent);
var
   Item : TMenuItem;
begin
   Item := NewItem(ACaption, scNone, False, True, AEvent, 0, '');
   MainMenu1.Items[1].Add(Item);
   with TSpeedButton.Create(panToolbar) do
   begin
      Top := 4;
      Left := 4+(NumButtons * Width);
      Parent := panToolbar;
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

end.
