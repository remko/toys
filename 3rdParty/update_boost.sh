if [ -z "$1" ]; then
	echo "Please specify the location of the boost source tree"
	exit -1
fi

TARGET_DIR=boost

if [ ! -d "$TARGET_DIR" ]; then
	mkdir $TARGET_DIR
fi

bcp --boost="$1" \
	coroutine/coroutine.hpp \
	smart_ptr/make_shared.hpp \
	context/src \
	$TARGET_DIR
cp $1/LICENSE_1_0.txt $TARGET_DIR
