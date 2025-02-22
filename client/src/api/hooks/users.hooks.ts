import { useQuery, useMutation } from "@tanstack/react-query";
import {
  createUser,
  deleteUser,
  getUser,
  getUsers,
} from "../client/services/users";
import { CreateUserRequest, User } from "../__generated__/types.gen";

export const useGetUsers = () => {
  return useQuery<User[], Error>({
    queryKey: ["users"],
    queryFn: getUsers,
  });
};

export const useGetUser = (userId: string) => {
  return useQuery<User, Error>({
    queryKey: ["user", userId],
    queryFn: async () => await getUser(userId),
  });
};

export const useCreateUser = () => {
  return useMutation({
    mutationFn: (newUser: CreateUserRequest) => {
      return Promise.resolve(createUser(newUser));
    },
  });
};

export const useDeleteUser = () => {
  return useMutation({
    mutationFn: (userId: string) => {
      return Promise.resolve(deleteUser(userId));
    },
  });
};
