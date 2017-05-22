//---------------------------------------------------------------------------

#ifndef JvThumbnailChildFormUH
#define JvThumbnailChildFormUH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "JvBaseThumbnail.hpp"
#include "JvExExtCtrls.hpp"
#include "JvThumbImage.hpp"
#include "JvThumbnails.hpp"
#include <ComCtrls.hpp>
#include <ExtCtrls.hpp>
#include <FileCtrl.hpp>
//---------------------------------------------------------------------------
class TJvThumbnailChildForm : public TForm
{
__published:	// IDE-managed Components
        TPanel *Panel7;
        TJvThumbImage *ThumbImage1;
        TPanel *Panel5;
        TLabel *Label5;
        TBevel *Bevel1;
        TCheckBox *asButtonCB;
        TCheckBox *autoloadCB;
        TCheckBox *minMemCB;
        TRadioGroup *titlePlaceGr;
        TEdit *Edit1;
        TGroupBox *GroupBox1;
        TButton *Button4;
        TButton *Button5;
        TButton *Button1;
        TRadioGroup *AngleGr;
        TJvThumbnail *ThumbNail1;
        TPanel *Panel6;
        TSplitter *Splitter4;
        TDirectoryListBox *DirectoryListBox2;
        TFileListBox *FileListBox1;
        TPanel *Panel8;
        TLabel *Label6;
        TLabel *Label7;
        TLabel *Label8;
        TLabel *Label9;
        TLabel *Label1;
        TTrackBar *RedBar;
        TTrackBar *GreenBar;
        TTrackBar *BlueBar;
        TTrackBar *contrastBar;
        TButton *Button2;
        TTrackBar *LightnessBar;
        TPanel *Panel9;
        TDriveComboBox *DriveComboBox2;
        TPanel *Panel10;
        TFilterComboBox *FilterComboBox1;
        void __fastcall FileListBox1Change(TObject *Sender);
        void __fastcall Button2Click(TObject *Sender);
        void __fastcall ThumbNail1Click(TObject *Sender);
        void __fastcall Button4Click(TObject *Sender);
        void __fastcall Button5Click(TObject *Sender);
        void __fastcall AngleGrClick(TObject *Sender);
        void __fastcall asButtonCBClick(TObject *Sender);
        void __fastcall autoloadCBClick(TObject *Sender);
        void __fastcall minMemCBClick(TObject *Sender);
        void __fastcall titlePlaceGrClick(TObject *Sender);
        void __fastcall FormShow(TObject *Sender);
        void __fastcall Panel6Resize(TObject *Sender);
        void __fastcall Panel8Resize(TObject *Sender);
        void __fastcall Panel10Resize(TObject *Sender);
private:	// User declarations
public:		// User declarations
        __fastcall TJvThumbnailChildForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
//extern PACKAGE TJvThumbnailChildForm *JvThumbnailChildForm;
//---------------------------------------------------------------------------
#endif
 