unit LanguageLoaderExampleForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, JvMenus, JvComponent, JvgLanguageLoader, StdCtrls,
  JvComponentBase;

type
  TForm1 = class(TForm)
    MainMenu: TJvMainMenu;
    MM_Bestand: TMenuItem;
    FileNew: TMenuItem;
    FileOpen: TMenuItem;
    FileSave: TMenuItem;
    FileExit: TMenuItem;
    MM_Bewerken: TMenuItem;
    MM_EditUndo: TMenuItem;
    Redo1: TMenuItem;
    N4: TMenuItem;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    MM_Help: TMenuItem;
    HelpShowHelp: TMenuItem;
    HelpAbout: TMenuItem;
    SelectLang: TMenuItem;
    LangEnglish: TMenuItem;
    LangDutch: TMenuItem;
    LangFrench: TMenuItem;
    JvgLanguageLoader: TJvgLanguageLoader;
    btnOK: TButton;
    btnCancel: TButton;
    lbl1: TLabel;
    lbl2: TLabel;
    lbl3: TLabel;
    lblWarning: TLabel;
    procedure FileExitClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LangDutchClick(Sender: TObject);
    procedure LangFrenchClick(Sender: TObject);
    procedure LangEnglishClick(Sender: TObject);
  private
    FLanguageDir: string;
    FLanguageFile: string;
    { Private declarations }
  public
    { Public declarations }
    property LanguageDir: string read FLanguageDir;
    property LanguageFile: string read FLanguageFile;

    procedure TranslateConsts;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  JclFileUtils, JclStrings;


// Some string constants
const
  BlueLabel1 = 'English';
  BlueLabel2 = 'Dutch';
  BlueLabel3 = 'French';

procedure TForm1.FileExitClick(Sender: TObject);
begin
  Close;
end;

procedure TForm1.FormShow(Sender: TObject);
var
  ProgramDir: string;
begin
  // Get program dir (in case it's running from a different dir)
  ProgramDir := PathExtractFileDirFixed(Application.ExeName);
  ProgramDir := StrEnsureSuffix('\', ProgramDir);
  FLanguageDir := ProgramDir + '..\examples\Globus\LanguageLoader';
  FLanguageDir := StrEnsureSuffix('\', PathCanonicalize(FLanguageDir));


  // In a real app this should be done only once and after that saved
  // in the programs's settings, here we do it always at startup...

  // Get the default system language
  if  (GetUserDefaultLangID and LANG_DUTCH) = LANG_DUTCH then
  begin
    // Set dutch as currently used language
    FLanguageFile := LanguageDir + 'Nederlands.lng';
    LangDutch.Checked := True;
  end
  else if (GetUserDefaultLangID and LANG_FRENCH) = LANG_FRENCH then
  begin
    // Set french as currently used language
    FLanguageFile := LanguageDir + 'Francais.lng';
    LangFrench.Checked := True;
  end
  else
  begin
    // use English as default
    // No real need to translate it but we do it anyway
    // The English language file is needed when changing from one language to another
    FLanguageFile := LanguageDir + 'English.lng';
    LangEnglish.Checked := True;
  end;

  JvgLanguageLoader.DictionaryFileName := LanguageFile;

  // Translate the form
  // The translations for the form to be translated will be found
  // in the ini section name defined in property JvgLanguageLoader.FormSection
  // When the second parameter is True string constants will also be loaded from
  // the section defined in JvgLanguageLoader.StringsSection
  jvgLanguageLoader.TranslateComponent(Self,True);

  // Now translate some string constants
  TranslateConsts;
end;

procedure TForm1.TranslateConsts;
begin
  // Translate some constant strings
  lbl1.Caption := jvgLanguageLoader.Translate(BlueLabel1);
  lbl2.Caption := jvgLanguageLoader.Translate(BlueLabel2);
  lbl3.Caption := jvgLanguageLoader.Translate(BlueLabel3);
end;

procedure TForm1.LangDutchClick(Sender: TObject);
var OldLanguage: string;
begin
  if not LangDutch.Checked then
  begin
    OldLanguage := LanguageFile;
    FLanguageFile := LanguageDir + 'Nederlands.lng';
    JvgLanguageLoader.DictionaryFileName := LanguageFile;

    // Change language of this form
    jvgLanguageLoader.ChangeTranslation(Self,True,OldLanguage);
    LangFrench.Checked := False;
    LangEnglish.Checked := False;
    LangDutch.Checked := True;

    // Now translate some string constants we are using on this form
    TranslateConsts;
  end;
end;

procedure TForm1.LangFrenchClick(Sender: TObject);
var OldLanguage: string;
begin
  if not LangFrench.Checked then
  begin
    OldLanguage := LanguageFile;
    FLanguageFile := LanguageDir+'Francais.lng';
    JvgLanguageLoader.DictionaryFileName := LanguageFile;

    // Change language of this form
    jvgLanguageLoader.ChangeTranslation(Self,True,OldLanguage);
    LangFrench.Checked := True;
    LangEnglish.Checked := False;
    LangDutch.Checked := False;

    // Now translate some string constants we are using on this form
    TranslateConsts;
  end;
end;

procedure TForm1.LangEnglishClick(Sender: TObject);
var OldLanguage: string;
begin
  if not LangEnglish.Checked then
  begin
    OldLanguage := LanguageFile;
    FLanguageFile := LanguageDir+'English.lng';
    JvgLanguageLoader.DictionaryFileName := LanguageFile;

    // Change language of this form
    jvgLanguageLoader.ChangeTranslation(Self,True,OldLanguage);
    LangFrench.Checked := False;
    LangEnglish.Checked := True;
    LangDutch.Checked := False;

    // Now translate some string constants we are using on this form
    TranslateConsts;
  end;
end;

end.
