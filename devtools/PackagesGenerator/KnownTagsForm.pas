unit KnownTagsForm;

{$I jvcl.inc}

interface

uses
  Windows, Messages, SysUtils, {$IFDEF COMPILER6_UP}Variants, {$ENDIF}Classes, Graphics, Controls, Forms,
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
    tshFilesAndForms: TTabSheet;
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
    lblNote: TLabel;
    lblDateTime: TLabel;
    lblDateTimeUsage: TLabel;
    lblXmlName: TLabel;
    lblXmlNameUsage: TLabel;
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
