unit Main;

{$I jvcl.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ImgList, ActnList, PDPackageLoader, JvSimpleXml;

type
  TFormMain = class(TForm)
    ImageListComponents: TImageList;
    OpenDialogPackages: TOpenDialog;
    TreeViewComponents: TTreeView;
    BtnAdd: TButton;
    Button2: TButton;
    ActionList1: TActionList;
    ActionAddPackage: TAction;
    ActionRemovePackage: TAction;
    ProgressBar: TProgressBar;
    LblComponentCount: TLabel;
    LblActionCount: TLabel;
    LblUnitCount: TLabel;
    BtnExport: TButton;
    SaveDialogExport: TSaveDialog;
    LblPackage: TLabel;
    procedure ActionAddPackageExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ActionRemovePackageUpdate(Sender: TObject);
    procedure ActionRemovePackageExecute(Sender: TObject);
    procedure BtnExportClick(Sender: TObject);
  private
    { Private-Deklarationen }
    procedure UpdateTreeView;
    procedure SaveToXml(xml: TJvSimpleXMLElem);
    procedure SavePackagesToXml(xml: TJvSimpleXMLElem);
  public
    { Public-Deklarationen }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

{$IFDEF COMPILER5}
function GetEnvironmentVariable(const Variable: string): string;
begin
  SetLength(Result, 4096 - 16);
  SetLength(Result, Windows.GetEnvironmentVariable(PChar(Variable), PChar(Result), Length(Result)));
end;
{$ENDIF COMPILER5}

procedure TFormMain.SaveToXml(xml: TJvSimpleXMLElem);
begin
  SavePackagesToXml(xml.Items.Add('Packages'));
end;

procedure TFormMain.SavePackagesToXml(xml: TJvSimpleXMLElem);

  procedure SavePackage(xml: TJvSimpleXMLElem; Package: IPackage);
  var
    i: Integer;
    xmlReqs: TJvSimpleXMLElem;
    xmlUnits: TJvSimpleXMLElem;
    xmlComps: TJvSimpleXMLElem;
    xmlActs: TJvSimpleXMLElem;
  begin
    xml.Properties.Add('Name', Package.Name);
    xml.Properties.Add('Description', Package.Description);
    xml.Properties.Add('DcpBpiName', Package.DcpBpiName);
    xml.Properties.Add('Flags', IntToHex(Package.Flags, 8));

    if Package.RequireCount > 0 then
    begin
      xmlReqs := xml.Items.Add('Requires');
      for i := 0 to Package.RequireCount - 1 do
      begin
        with xmlReqs.Items.Add('Require') do
          Properties.Add('Name', Package.Requires[i]);
      end;
    end;

    if Package.UnitCount > 0 then
    begin
      xmlUnits := xml.Items.Add('Units');
      for i := 0 to Package.UnitCount - 1 do
      begin
        with xmlUnits.Items.Add('Unit') do
        begin
          Properties.Add('Name', Package.Units[i].Name);
          Properties.Add('Flags', IntToHex(Package.Units[i].Flags, 8));
        end;
      end;
    end;

    if Package.ComponentCount > 0 then
    begin
      xmlComps := xml.Items.Add('Components');
      for i := 0 to Package.ComponentCount - 1 do
      begin
        if Package.Components[i].ComponentClass.ClassName = 'TJvPageManager' then
          Write;
        with xmlComps.Items.Add('Component') do
        begin
          Properties.Add('Palette', Package.Components[i].Palette);
          Properties.Add('ClassName', Package.Components[i].ComponentClass.ClassName);
          Properties.Add('ImageIndex', Package.Components[i].ImageIndex);
          Properties.Add('UnitName', Package.Components[i].UnitName);
        end;
      end;
    end;

    if Package.ActionCount > 0 then
    begin
      xmlActs := xml.Items.Add('Actions');
      for i := 0 to Package.ActionCount - 1 do
      begin
        with xmlActs.Items.Add('Action') do
        begin
          Properties.Add('Category', Package.Actions[i].Category);
          Properties.Add('ClassName', Package.Actions[i].ActionClass.ClassName);
          if Package.Actions[i].Resource <> nil then
            Properties.Add('Resource', Package.Actions[i].Resource.ClassName);
          Properties.Add('UnitName', Package.Actions[i].UnitName);
        end;
      end;
    end;

  end;

var
  i: Integer;
begin
  for i := 0 to PackageLoader.PackageCount - 1 do
    SavePackage(xml.Items.Add('Package'), PackageLoader.Packages[i]);
end;

procedure TFormMain.UpdateTreeView;
var
  Item: IComponentItem;
  Pkg: IPackage;
  ParentNode, Node: TTreeNode;
  i, k: Integer;
begin
  TreeViewComponents.Items.BeginUpdate;
  try
    TreeViewComponents.Items.Clear;
    for i := 0 to PackageLoader.PackageCount - 1 do
    begin
      Pkg := PackageLoader.Packages[i];
      ParentNode := TreeViewComponents.Items.Add(nil, Pkg.Name);
      ParentNode.ImageIndex := -1;
      ParentNode.SelectedIndex := ParentNode.ImageIndex;
      for k := 0 to Pkg.ComponentCount - 1 do
      begin
        Item := Pkg.Components[k];
        if Item.Palette = sNoIconPalette then
          Continue;
        Node := TreeViewComponents.Items.AddChild(ParentNode, Item.ComponentClass.ClassName);
        Node.ImageIndex := Item.ImageIndex;
        Node.SelectedIndex := Node.ImageIndex;
      end;
    end;
  finally
    TreeViewComponents.Items.EndUpdate;
  end;
end;

procedure TFormMain.ActionAddPackageExecute(Sender: TObject);
var
  i: Integer;
begin
  if OpenDialogPackages.Execute then
  begin
    ProgressBar.Position := 0;
    ProgressBar.Max := OpenDialogPackages.Files.Count;
    for i := 0 to OpenDialogPackages.Files.Count - 1 do
    begin
      if PackageLoader.IndexOf(ChangeFileExt(ExtractFileName(OpenDialogPackages.Files[i]), '')) = -1 then
      begin
        LblPackage.Caption := ChangeFileExt(ExtractFileName(OpenDialogPackages.Files[i]), '');
        try
          PackageLoader.AddPackage(OpenDialogPackages.Files[i]);
        except
          Application.HandleException(Self);
        end;
        ProgressBar.StepBy(1);
        LblComponentCount.Caption := 'Components: ' + IntToStr(PackageLoader.ComponentCount);
        LblActionCount.Caption := 'Actions: ' + IntToStr(PackageLoader.ActionCount);
        LblUnitCount.Caption := 'Units: ' + IntToStr(PackageLoader.UnitCount);
        Application.ProcessMessages;
      end;
    end;

    UpdateTreeView;
  end;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  SetEnvironmentVariable('PATH', 
    {$IFDEF DELPHI5}
    PChar('C:\Borland\Delphi5\Bin;' + 
    {$ENDIF DELPHI5}
    {$IFDEF DELPHI6}
    PChar('C:\Borland\Delphi6\Bin;' + 
    {$ENDIF DELPHI6}
    {$IFDEF DELPHI7}
    PChar('C:\Borland\Delphi7\Bin;' + 
    {$ENDIF DELPHI7}
    {$IFDEF DELPHI9}
    PChar('C:\Borland\BDS\3.0\Bin;' + 
    {$ENDIF DELPHI9}
    GetEnvironmentVariable('PATH')));
  PackageLoader := TPackageLoader.Create(Self);
  PackageLoader.ImageList := ImageListComponents;
end;

procedure TFormMain.ActionRemovePackageUpdate(Sender: TObject);
begin
  ActionRemovePackage.Enabled := (TreeViewComponents.Selected <> nil) and
    (TreeViewComponents.Selected.Level = 0);
end;

procedure TFormMain.ActionRemovePackageExecute(Sender: TObject);
begin
  PackageLoader.RemovePackage(TreeViewComponents.Selected.Index);
  TreeViewComponents.Selected.Delete;
end;

procedure TFormMain.BtnExportClick(Sender: TObject);
var
  Doc: TJvSimpleXML;
  Bmp: TBitmap;
begin
  if SaveDialogExport.Execute then
  begin
    Doc := TJvSimpleXML.Create(nil);
    try
      SaveToXml(Doc.Root);
      Doc.SaveToFile(SaveDialogExport.FileName);

      Bmp := TBitmap.Create;
      try
        Bmp.Handle := ImageListComponents.GetImageBitmap;
        Bmp.SaveToFile(ChangeFileExt(SaveDialogExport.FileName, '.bmp'));
        Bmp.Handle := 0;
      finally
        Bmp.Free;
      end;
    finally
      Doc.Free;
    end;
  end;
end;

end.
