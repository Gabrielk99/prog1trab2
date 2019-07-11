for base in Base\ de\ Exemplos/*
do
    if [ -d "$base" ] ; then
        echo "---------------------------------------------"
        echo "Testando na $base..."
        cp "$base/descricao.txt" ./descricao.txt
        cp "$base/base.txt" base.txt
        cp "$base/caso.txt" caso.txt
        cp "$base/result.txt" resposta.txt

        shuf base.txt --random-source=base.txt > base_embaralhada.txt
        split -l $[ $(wc -l base.txt |cut -d" " -f1) * 80 / 100 ] base_embaralhada.txt
        mv xaa base.txt
        awk 'NF{NF-=1};1' < xab > caso.txt
        awk '{print $NF}' < xab > resposta.txt
        echo "Tempo gasto:"
        time ./main
        erros=`diff -w classe.txt resposta.txt | grep "^>" | wc -l`
        total=`wc -l < caso.txt`
        acuracia=$(echo "scale=2; 100 - (($erros * 100) /$total)" | bc)
        echo "Acuracia: $acuracia%"
    fi
done

rm descricao.txt
rm base.txt
rm caso.txt
rm resposta.txt
rm xab