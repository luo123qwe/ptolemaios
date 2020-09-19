/////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Tencent is pleased to support the open source community by making behaviac available.
//
// Copyright (C) 2015-2017 THL A29 Limited, a Tencent company. All rights reserved.
//
// Licensed under the BSD 3-Clause License (the "License"); you may not use this file except in compliance with
// the License. You may obtain a copy of the License at http://opensource.org/licenses/BSD-3-Clause
//
// Unless required by applicable law or agreed to in writing, software distributed under the License is
// distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and limitations under the License.
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#include <erl_nif.h>
#include "behaviac_generated/types/behaviac_types.h"

#if BEHAVIAC_CCDEFINE_ANDROID
#include <android/log.h>

#define LOGI(...) ((void)__android_log_print(ANDROID_LOG_INFO, "behaviac_enif", __VA_ARGS__))
#else
#define LOGI printf

#if BEHAVIAC_CCDEFINE_MSVC
#include <windows.h>
#include <tchar.h>
#endif

#endif

#if !BEHAVIAC_CCDEFINE_ANDROID
static void SetExePath()
{
#if BEHAVIAC_CCDEFINE_MSVC
	TCHAR szCurPath[_MAX_PATH];

	GetModuleFileName(NULL, szCurPath, _MAX_PATH);

	TCHAR* p = szCurPath;

	while (_tcschr(p, L'\\'))
	{
		p = _tcschr(p, L'\\');
		p++;
	}

	*p = L'\0';

	SetCurrentDirectory(szCurPath);
#endif
}
#endif

FirstAgent* g_FirstAgent = NULL;

bool InitBehavic()
{
	LOGI("InitBehavic\n");

	behaviac::Workspace::GetInstance()->SetFilePath("./behaviac");

	behaviac::Workspace::GetInstance()->SetFileFormat(behaviac::Workspace::EFF_xml);

    return true;
}

bool InitPlayer()
{
	LOGI("InitPlayer\n");

	g_FirstAgent = behaviac::Agent::Create<FirstAgent>();

	bool bRet = g_FirstAgent->btload("FirstBT");

	g_FirstAgent->btsetcurrent("FirstBT");

    return bRet;
}

void UpdateLoop()
{
	LOGI("UpdateLoop\n");

	int frames = 0;
	behaviac::EBTStatus status = behaviac::BT_RUNNING;

	while (status == behaviac::BT_RUNNING)
	{
		LOGI("frame %d\n", ++frames);
        behaviac::Workspace::GetInstance()->DebugUpdate();

		status = g_FirstAgent->btexec();
	}
}

void CleanupPlayer()
{
	LOGI("CleanupPlayer\n");

	behaviac::Agent::Destroy(g_FirstAgent);
}

void CleanupBehaviac()
{
	LOGI("CleanupBehaviac\n");

	behaviac::Workspace::GetInstance()->Cleanup();
}

#if !BEHAVIAC_CCDEFINE_ANDROID
int test()
{
#if BEHAVIAC_RELEASE
printf("BEHAVIAC_RELEASE");
#endif
	behaviac::Config::SetLogging(true);
	behaviac::Config::SetSocketing(false);
	behaviac::Config::SetHotReload(false);

	LOGI("BEHAVIAC_CCDEFINE_NAME=%s\n", BEHAVIAC_CCDEFINE_NAME);

    InitBehavic();

    InitPlayer();

    UpdateLoop();

    CleanupPlayer();

    CleanupBehaviac();

    return 0;
}

#endif



static ERL_NIF_TERM run(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    test();
    return enif_make_atom(env, "ok");
}

static ErlNifFunc nif_funcs[] =
{
    {"run", 0, run}
};

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    printf("behaviac_enif call load\n");
    return 0;
}

static int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info)
{
    printf("behaviac_enif call upgrade\n");
    return 0;
}

static void unload(ErlNifEnv* env, void* priv_data)
{
    printf("behaviac_enif call unload\n");
}

ERL_NIF_INIT(behaviac_enif, nif_funcs, load, NULL, upgrade, unload)