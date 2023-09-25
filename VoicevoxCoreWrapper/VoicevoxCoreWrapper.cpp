#include "pch.h"

#pragma comment(lib, "voicevox_core/voicevox_core.lib")

#define DLL_EXPORT extern "C" __declspec(dllexport)

DLL_EXPORT int __cdecl InitializeVoicevox()
{
	VoicevoxInitializeOptions options = voicevox_make_default_initialize_options();
	options.open_jtalk_dict_dir = "open_jtalk_dic_utf_8-1.11";
	options.load_all_models = true;
	return voicevox_initialize(options);
}

DLL_EXPORT void __cdecl FinalizeVoicevox()
{
	voicevox_finalize();
}

DLL_EXPORT int __cdecl TextToSpeech(const wchar_t *text, int speaker_id, uintptr_t *length, uint8_t **wav)
{
	std::wstring_convert<std::codecvt_utf8_utf16<wchar_t>> cvt;
	std::string str = cvt.to_bytes(text);
	VoicevoxTtsOptions options = voicevox_make_default_tts_options();
	return voicevox_tts(str.c_str(), speaker_id, options, length, wav);
}

DLL_EXPORT void __cdecl FreeWav(uint8_t *wav)
{
	voicevox_wav_free(wav);
}

DLL_EXPORT void __cdecl GetMetasJson()
{
	const char *chars = voicevox_get_metas_json();

	std::fwrite(chars, sizeof(char), std::strlen(chars), stdout);
}