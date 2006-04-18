unit FrmStartup;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, JvSimpleXml, JvGIF, Jpeg;

const
  WM_SHOWN = WM_USER + 1;

type
  TFormStartup = class(TForm)
    PanelClient: TPanel;
    ProgressBar: TProgressBar;
    PanelInner: TPanel;
    LblComponents: TLabel;
    LblPackages: TLabel;
    LblComponentName: TLabel;
    LblUnits: TLabel;
    PaintBox: TPaintBox;
    Image: TImage;
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
  private
    { Private-Deklarationen }
    procedure LoadPackageFromXml(xml: TJvSimpleXMLElem);
    procedure WMShown(var Msg: TMessage); message WM_SHOWN;
    procedure WMEraseBkgnd(var Msg: TMessage); message WM_ERASEBKGND;
  public
    { Public-Deklarationen }
  end;

var
  FormStartup: TFormStartup;

implementation

uses
  DataModuleMain, Packages, Configuration;

{$R *.dfm}

function HexToInt(const Value: string): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Length(Value) do
  begin
    case Value[i] of
      '0'..'9':
        Result := Result shl 4 + Ord(Value[i]) - Ord('0');
      'A'..'F':
        Result := Result shl 4 + Ord(Value[i]) - Ord('A');
      'a'..'f':
        Result := Result shl 4 + Ord(Value[i]) - Ord('a');
    end;
  end;
end;

procedure TFormStartup.LoadPackageFromXml(xml: TJvSimpleXMLElem);
var
  Name: string;
  Description: string;
  DcpBpiName: string;
  Flags: Integer;
  Package: TPackage;

  i: Integer;
  xmlReqs: TJvSimpleXMLElem;
  xmlUnits: TJvSimpleXMLElem;
  xmlComps: TJvSimpleXMLElem;
  xmlActs: TJvSimpleXMLElem;
  Comp: TComponentItem;
  Pkg: IPackage;
  R: TRect;
begin
  R := Rect(PanelClient.ClientWidth - 5 - 24, 5, PanelClient.ClientWidth - 5, 5 + 24);

  Name := xml.Properties.Value('Name', '');
  Description := xml.Properties.Value('Description', '');
  DcpBpiName := xml.Properties.Value('DcpBpiName', '');
  Flags := HexToInt(xml.Properties.Value('Flags', '0'));

  if Name <> '' then
  begin
    Package := TPackage.Create(PackageList, Name, Description, DcpBpiName, Flags);
    Pkg := Package;
    PackageList.AddPackage(Pkg);

    { read requires }
    xmlReqs := xml.Items.ItemNamed['Requires'];
    if xmlReqs <> nil then
      for i := 0 to xmlReqs.Items.Count - 1 do
      begin
        if xmlReqs.Items[i].Name = 'Require' then
          Package.AddRequire(
            xmlReqs.Items[i].Properties.Value('Name', '')
          );
      end;

    { read units }
    xmlUnits := xml.Items.ItemNamed['Units'];
    if xmlUnits <> nil then
      for i := 0 to xmlUnits.Items.Count - 1 do
      begin
        if xmlUnits.Items[i].Properties.Value('Name', '') = 'SysInit' then
          Continue;
        if xmlUnits.Items[i].Name = 'Unit' then
          Package.AddUnit(TUnit.Create(
            xmlUnits.Items[i].Properties.Value('Name', ''),
            HexToInt(xmlUnits.Items[i].Properties.Value('Flags', '0'))
          ));
      end;

    { read components }
    PaintBox.Canvas.Brush.Color := clBtnFace;
    xmlComps := xml.Items.ItemNamed['Components'];
    if xmlComps <> nil then
      for i := 0 to xmlComps.Items.Count - 1 do
      begin
        if xmlComps.Items[i].Name = 'Component' then
        begin
          Comp := TComponentItem.Create(
            Pkg,
            xmlComps.Items[i].Properties.Value('Palette', ''),
            xmlComps.Items[i].Properties.Value('ClassName', ''),
            xmlComps.Items[i].Properties.IntValue('ImageIndex', 1),
            xmlComps.Items[i].Properties.Value('UnitName', '')
          );
          Package.AddComponent(Comp);
          LblComponentName.Caption := Comp.ComponentClass;
          LblComponentName.Update;

          PaintBox.Canvas.FillRect(PaintBox.ClientRect);
          DMMain.ImageListComponents.Draw(PaintBox.Canvas, 0, 0, Comp.ImageIndex);
        end;
      end;

    { read actions }
    xmlActs := xml.Items.ItemNamed['Actionss'];
    if xmlActs <> nil then
      for i := 0 to xmlComps.Items.Count - 1 do
      begin
        if xmlActs.Items[i].Name = 'Actions' then
          Package.AddAction(TActionItem.Create(
            xmlActs.Items[i].Properties.Value('Category', ''),
            xmlActs.Items[i].Properties.Value('ClassName', ''),
            xmlActs.Items[i].Properties.Value('Resource', ''),
            xmlComps.Items[i].Properties.Value('UnitName', '')
          ));
      end;

  end;
end;

procedure TFormStartup.WMShown(var Msg: TMessage);
var
  Doc: TJvSimpleXML;
  Bmp, SliceBmp, MaskBmp: TBitmap;
  Gif: TJvGIFImage;
  i, k: Integer;
  x, y, w, h: Integer;
  UnitItem: IUnit;
begin
  Doc := TJvSimpleXML.Create(nil);
  try
    Doc.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'Config\packages.xml');

    ProgressBar.Position := 0;
    ProgressBar.Max := Doc.Root.Items.Count + 2;
    Application.ProcessMessages;
    Bmp := TBitmap.Create;
    try
      Gif := TJvGIFImage.Create;
      try
        Gif.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'Config\packages.gif');
        Bmp.Assign(Gif);
      finally
        Gif.Free;
      end;
      //Bmp.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'Config\packages.bmp');

      { fill image list }
      DMMain.ImageListComponents.Clear;
      w := DMMain.ImageListComponents.Width;
      h := DMMain.ImageListComponents.Height;
      SliceBmp := TBitmap.Create;
      MaskBmp := TBitmap.Create;
      try
        SliceBmp.Width := w;
        SliceBmp.Height := h;
        for y := 0 to Bmp.Height div h - 1 do
          for x := 0 to Bmp.Width div w - 1 do
          begin
            SliceBmp.Canvas.CopyRect(Rect(0, 0, w, h), Bmp.Canvas, Rect(x * w, y * h, (x + 1) * w, (y + 1) * h));
            SliceBmp.Transparent := True;
            MaskBmp.Handle := SliceBmp.MaskHandle;
            DMMain.ImageListComponents.Add(SliceBmp, MaskBmp);
            MaskBmp.Handle := 0;
          end;
      finally
        MaskBmp.Free;
        SliceBmp.Free;
      end;
    finally
      Bmp.Free;
    end;
    ProgressBar.StepBy(1);

    LblComponentName.Caption := '';
    LblPackages.Caption := 'Package count: 0';
    LblComponents.Caption := 'Component count: 0';
    LblUnits.Caption := 'Unit count: 0';
    LblComponentName.Visible := True;
    Application.ProcessMessages;
    for i := 0 to Doc.Root.Items.Count - 1 do
    begin
      if Doc.Root.Items[i].Name = 'Package' then
        LoadPackageFromXml(Doc.Root.Items[i]);
      LblPackages.Caption := 'Package count: ' + IntToStr(PackageList.PackageCount);
      LblComponents.Caption := 'Component count: ' + IntToStr(PackageList.ComponentCount);
      LblUnits.Caption := 'Unit count: ' + IntToStr(PackageList.UnitCount);
      ProgressBar.StepBy(1);
      Application.ProcessMessages;
    end;
  finally
    Doc.Free;
  end;

  LblComponentName.Caption := '';
  PaintBox.Visible := False;
  LblComponentName.Update;
  { Fill Unit.Components[] }
  for i := 0 to PackageList.UnitCount - 1 do
  begin
    UnitItem := PackageList.Units[i];
    for k := 0 to PackageList.ComponentCount - 1 do
      if CompareText(UnitItem.Name, PackageList.Components[k].UnitName) = 0 then
        UnitItem.AddComponent(PackageList.Components[k]);
  end;
  ProgressBar.StepBy(1);
  Application.ProcessMessages;

  Release;
end;

procedure TFormStartup.FormShow(Sender: TObject);
begin
  PostMessage(Handle, WM_SHOWN, 0, 0);
end;

procedure TFormStartup.WMEraseBkgnd(var Msg: TMessage);
begin
  Msg.Result := 1;
end;

procedure TFormStartup.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := False;
end;

procedure TFormStartup.FormCreate(Sender: TObject);
var
  h: Integer;
  R: TRect;
begin
  if FileExists('Config\' + Config.StartupPicture) then
  begin
    h := ClientHeight;
    Image.Align := alNone;
    Image.Picture.LoadFromFile('Config\' + Config.StartupPicture);
    if Image.Width > ClientWidth then
      ClientWidth := Image.Width;
    ClientHeight := h + Image.Height;
    Image.Align := alTop;
  end;
end;

initialization
  PackageList := TPackageList.Create(nil);

finalization
  FreeAndNil(PackageList);

end.
