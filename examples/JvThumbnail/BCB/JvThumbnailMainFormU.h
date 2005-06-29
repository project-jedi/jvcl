//---------------------------------------------------------------------------

#ifndef JvThumbnailMainFormUH
#define JvThumbnailMainFormUH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "JvBaseThumbnail.hpp"
#include "JvCombobox.hpp"
#include "JvComponent.hpp"
#include "JvDriveCtrls.hpp"
#include "JvExControls.hpp"
#include "JvExForms.hpp"
#include "JvExStdCtrls.hpp"
#include "JvListBox.hpp"
#include "JvSpecialProgress.hpp"
#include "JvThumbViews.hpp"
#include <ComCtrls.hpp>
#include <ExtCtrls.hpp>
#include "JvExMask.hpp"
#include "JvSpin.hpp"
#include <Mask.hpp>
//---------------------------------------------------------------------------
class TJvThumbnailMainForm : public TForm
{
__published:	// IDE-managed Components
        TPageControl *PageControl1;
        TTabSheet *TabSheet1;
        TSplitter *Splitter1;
        TPanel *Panel1;
        TLabel *Label1;
        TLabel *Label2;
        TLabel *Label3;
        TLabel *Label4;
        TTrackBar *TrackBar1;
        TJvDriveCombo *DriveComboBox1;
        TCheckBox *CheckBox1;
        TCheckBox *CheckBox2;
        TJvSpinEdit *SpinEdit1;
        TJvSpinEdit *SpinEdit2;
        TCheckBox *CheckBox3;
        TCheckBox *CheckBox4;
        TButton *Button1;
        TButton *Button2;
        TPanel *Panel2;
        TJvDirectoryListBox *DirectoryListBox1;
        TRadioGroup *RadioGroup1;
        TRadioGroup *RadioGroup2;
        TPanel *Panel3;
        TPanel *Panel4;
        TJvThumbView *thumbView1;
        TPanel *Panel5;
        TBevel *Bevel1;
        TJvSpecialProgress *JvSpecialProgress1;
        void __fastcall DirectoryListBox1Change(TObject *Sender);
        void __fastcall RadioGroup1Click(TObject *Sender);
        void __fastcall thumbView1Change(TObject *Sender);
        void __fastcall thumbView1DblClick(TObject *Sender);
        void __fastcall thumbView1KeyUp(TObject *Sender, WORD &Key,
          TShiftState Shift);
        void __fastcall thumbView1MouseUp(TObject *Sender,
          TMouseButton Button, TShiftState Shift, int X, int Y);
        void __fastcall thumbView1ScanProgress(TObject *Sender,
          int Position, bool &Stop);
        void __fastcall thumbView1StartScanning(TObject *Sender, int Max);
        void __fastcall thumbView1StopScanning(TObject *Sender);
        void __fastcall TrackBar1Change(TObject *Sender);
        void __fastcall CheckBox1Click(TObject *Sender);
        void __fastcall CheckBox2Click(TObject *Sender);
        void __fastcall CheckBox4Click(TObject *Sender);
        void __fastcall RadioGroup2Click(TObject *Sender);
        void __fastcall Button1Click(TObject *Sender);
        void __fastcall Button2Click(TObject *Sender);
        void __fastcall SpinEdit1Change(TObject *Sender);
        void __fastcall SpinEdit2Change(TObject *Sender);
        void __fastcall FormShow(TObject *Sender);
private:        // User declarations
public:         // User declarations
        __fastcall TJvThumbnailMainForm(TComponent* Owner);
       bool NewDir;
       bool Scanning;

};
//---------------------------------------------------------------------------
extern PACKAGE TJvThumbnailMainForm *JvThumbnailMainForm;
//---------------------------------------------------------------------------
#endif
