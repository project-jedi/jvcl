unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, JvPluginMan, ExtCtrls, JvComponent;

type
  TfrmMain = class(TForm)
    JvPluginManager1: TJvPluginManager;
    Memo1: TMemo;
    Panel1: TPanel;
    butLoadPlugins: TButton;
    lstPlugins: TListBox;
    butShowPlug: TButton;
    butUnload: TButton;
    Splitter1: TSplitter;
    procedure butLoadPluginsClick(Sender: TObject);
    procedure butShowPlugClick(Sender: TObject);
    procedure JvPluginManager1ErrorLoading(Sender: TObject; s: String);
    procedure butUnloadClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  frmMain: TfrmMain;

implementation

uses JvPlugin, JvPlgIntf;

{$R *.DFM}

procedure TfrmMain.butLoadPluginsClick(Sender: TObject);
var counter : integer;
    Plug : TJvPlugin;
    MyPlug : IMyPlugInInterface;
begin
     butLoadPlugins.Enabled:=False;

     // Loading Plugins
     Memo1.LInes.Add('Looking in '+ExtractFilePath(Application.Exename)+' for Plugins');
     JvPluginManager1.LoadPlugins;
     Memo1.Lines.Add(IntToStr(JvPluginManager1.PluginCount)+' plugins loaded.');

     // Check if plugins have correct interface
     counter:=0;
     while counter < JvPluginManager1.PluginCount do
     begin
          Plug:=JvPluginManager1.Plugins[counter];
          if Plug.GetInterface(IMyPlugInInterface, MyPlug) then
          begin
               Memo1.LInes.Add(IntToStr(counter)+' : '+Plug.Description+' V '+Plug.PluginVersion);
               lstPlugins.Items.Add(Plug.Description+' V '+Plug.PluginVersion);

               Plug.Configure;
               Inc(counter);
          end
          else
          begin
               Memo1.Lines.Add('Warning : Plugin '+Plug.Description+' has unknown interface - unloaded!');
               JvPluginManager1.UnloadPlugin(counter);
          end;
     end;
end;

procedure TfrmMain.butShowPlugClick(Sender: TObject);
var MyPlug : IMyPlugInInterface;
begin
     if (lstPlugins.ItemIndex<0) or
        (lstPlugins.ItemIndex>lstPlugins.Items.Count)
     then
         Exit;

     JvPluginManager1.Plugins[lstPlugins.ItemIndex].GetInterface(IMyPlugInInterface, MyPlug);
     MyPlug.ShowPlug(self);
end;

procedure TfrmMain.JvPluginManager1ErrorLoading(Sender: TObject;
  s: String);
begin
     Memo1.Lines.Add(s);
end;

procedure TfrmMain.butUnloadClick(Sender: TObject);
begin
      while JvPluginManager1.PluginCount<>0 do
            JvPluginManager1.UnloadPlugin(0);

      lstPlugins.Clear;

      butLoadPlugins.Enabled:=True;
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
     butUnloadClick(nil);
end;

end.
