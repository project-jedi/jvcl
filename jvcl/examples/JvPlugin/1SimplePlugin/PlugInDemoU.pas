unit PlugInDemoU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvPluginMan, StdCtrls, Buttons, Menus, ExtCtrls, ImgList, JvComponent;

type
  TForm1 = class(TForm)
    uilPluginManager: TJvPluginManager;
    clbPlugins: TListBox;
    MainMenu1: TMainMenu;
    Plugin1: TMenuItem;
    lbStatus: TListBox;
    Panel1: TPanel;
    Panel2: TPanel;
    Label1: TLabel;
    labAuthor: TLabel;
    Label2: TLabel;
    labDescription: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    File1: TMenuItem;
    Exit1: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    SendMessagetoPlugins1: TMenuItem;
    N1: TMenuItem;
    Splitter1: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure uilPluginManagerBeforeLoad(Sender: TObject; Filename: String;
      var AllowLoad: Boolean);
    procedure uilPluginManagerBeforeLoading(Sender: TObject);
    procedure uilPluginManagerAfterLoading(Sender: TObject);
    procedure uilPluginManagerAfterLoad(Sender: TObject; Filename: String);
    procedure clbPluginsClick(Sender: TObject);
    procedure clbPluginsDblClick(Sender: TObject);
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

procedure TForm1.uilPluginManagerBeforeLoad(Sender: TObject;
  Filename: String; var AllowLoad: Boolean);
begin
   lbStatus.Items.Add('Loading Plug-in: ' + Filename);
end;

procedure TForm1.uilPluginManagerBeforeLoading(Sender: TObject);
begin
   lbStatus.Items.Add('Starting to load Plug-ins');
end;

procedure TForm1.uilPluginManagerAfterLoading(Sender: TObject);
begin
//   uilPluginManager.GetLoadedPlugins(clbPlugins.Items);
   lbStatus.Items.Add('Finished loading Plug-ins');
end;

procedure TForm1.uilPluginManagerAfterLoad(Sender: TObject;
  Filename: String);
begin
   uilPluginManager.GetLoadedPlugins(clbPlugins.Items);
   lbStatus.Items.Add('Finished loading Plug-in: ' + Filename);
end;

procedure TForm1.clbPluginsClick(Sender: TObject);
begin
   if clbPlugins.ItemIndex = -1 then Exit;
   labAuthor.Caption := uilPluginManager.Plugins[clbPlugins.ItemIndex].Author;
   labDescription.Caption := uilPluginManager.Plugins[clbPlugins.ItemIndex].Description;
end;

procedure TForm1.clbPluginsDblClick(Sender: TObject);
begin
   uilPluginManager.Plugins[clbPlugins.ItemIndex].Configure;
end;

procedure TForm1.Exit1Click(Sender: TObject);
begin
   Close;
end;

procedure TForm1.About1Click(Sender: TObject);
begin
   ShowMessage('A simple host application for demoing JEDI Plug-ins.'#13#10#13#10'(c) 2002, Project JEDI.');
end;

procedure TForm1.uilPluginManagerNewCommand(Sender: TObject; ACaption,
  AHint, AData: String; ABitmap: TBitmap; AEvent: TNotifyEvent);
var
   Item : TMenuItem;
begin
   lbStatus.Items.Add('Adding command: ' + ACaption);
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
