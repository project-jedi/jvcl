program RxGIFAnm;

uses
  Forms,
  JvExceptionForm,
  GifMain in 'GIFMAIN.PAS' {AnimatorForm},
  GifPrvw in 'GIFPRVW.PAS' {PreviewForm},
  GIFPal in 'GIFPal.pas' {PaletteForm};

{$R *.RES}

begin
  JvErrorIntercept;
  Application.Title := 'RX GIF Animator';
  Application.CreateForm(TAnimatorForm, AnimatorForm);
  Application.Run;
end.
