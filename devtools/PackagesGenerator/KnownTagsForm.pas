unit KnownTagsForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ComCtrls;

type
  TfrmKnownTags = class(TForm)
    bbtOk: TBitBtn;
    lblStartRequires: TLabel;
    lblEndRequires: TLabel;
    lblEndFiles: TLabel;
    lblStartFiles: TLabel;
    lblEndForms: TLabel;
    lblStartForms: TLabel;
    lblRequiresUsage: TLabel;
    lblFilesUsage: TLabel;
    lblFormsUsage: TLabel;
    lblC5PFlags: TLabel;
    lblC5PFlagsUsage: TLabel;
    lblC6PFlags: TLabel;
    lblC6PFlagsUsage: TLabel;
    lblType: TLabel;
    lblTypeUsage: TLabel;
    lblSmallType: TLabel;
    lblSmallTypeUsage: TLabel;
    lblName: TLabel;
    lblNameUsage: TLabel;
    lblDescription: TLabel;
    lblDescriptionUsage: TLabel;
    pctSections: TPageControl;
    tshOutside: TTabSheet;
    tshPackages: TTabSheet;
    tshFiles: TTabSheet;
    tshForms: TTabSheet;
    lblPackName: TLabel;
    lblPackNameUsage: TLabel;
    lblFilename: TLabel;
    lblFilenameUsage: TLabel;
    lblUnitName: TLabel;
    lblUnitNameUsage: TLabel;
    lblFormName: TLabel;
    lblFormNameUsage: TLabel;
    lblFormType: TLabel;
    lblFormTypeUsage: TLabel;
    lblFormNameAndType: TLabel;
    lblFormNameAndTypeUsage: TLabel;
    lblUnitNameSmall: TLabel;
    lblUnitNameSmallUsage: TLabel;
    lblFormPathName: TLabel;
    lblFormPathNameUsage: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmKnownTags: TfrmKnownTags;

implementation

{$R *.dfm}

end.
