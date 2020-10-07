function [L,U,y,x]=jsjjs2020214245(A, b)
% Doolittle
% Author: 肖文韬 计算机技术 2020214245
	% A = [2 2 3; 4 7 7; -2 4 5];
	% b = [1 2 1]';

	if ~isnumeric(A)
		error 'A 必须是数值矩阵'
	end

	[nRow, nCol] = size(A);
	if nRow ~= nCol
		error 'A 必须是方阵'
	end
	if nRow ~= size(b, 1)
		error 'A 和 b 必须行数相同'
	end

	U = zeros(nRow);
	L = diag(ones(nRow, 1));

	% 得出 U 的第一行元素
	U(1, :) = A(1, :);
	% 得出 L 的第一列元素
	L(:, 1) = A(:, 1) / U(1, 1);

	for k=2:nRow
		% 利用 A 的第 k 行计算 U 的第 k 行
		for j=k:nRow
			U(k, j) = A(k, j) - L(k, 1:(k-1)) * U(1:(k-1), j);
		end
		for i=(k+1):nRow
		% 利用 A 的第 k 列计算 L 的第 k 列
			L(i, k) = (A(i, k) - L(i, 1:(k-1)) * U(1:(k-1), k)) / U(k, k);
		end
	end

	if A ~= L * U
		error 'A 不能用Doolittle分解方法'
	end

	% disp(L);
	% disp(U);

	y = zeros(nRow, 1);
	y(1) = b(1);
	for i=2:nRow
		y(i) = b(i) - L(i, 1:(i-1)) * y(1:(i-1), 1);
	end
	x = zeros(nRow, 1);
	x(nRow) = y(nRow) / U(nRow, nRow);
	for i=(nRow-1):-1:1
		x(i) = (y(i) - U(i, (i+1):nRow) * x((i+1):nRow, 1)) / U(i, i);
	end

	% disp(x);
end
